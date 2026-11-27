package h8io.reflect.internal

import h8io.reflect.{Kind1, Kind2, Kind3, Type as RType}

import scala.collection.mutable
import scala.quoted.*

/** The Scala 3 macros behind `Type`, `Kind1`, `Kind2` and `Kind3`. Not part of the API. */
object Macros {
  def tpe[T: Type](using Quotes): Expr[RType[T]] = {
    val encoder = new Encoder(Type.show[T])
    val root = encoder.root[T]
    '{ Spec.tpe[T](${ encoder.table }, ${ lift(root) }) }
  }

  def kind1[F[_]: Type](using Quotes): Expr[Kind1[F]] = '{ Spec.kind1[F](${ kind[F] }) }
  def kind2[F[_, _]: Type](using Quotes): Expr[Kind2[F]] = '{ Spec.kind2[F](${ kind[F] }) }
  def kind3[F[_, _, _]: Type](using Quotes): Expr[Kind3[F]] = '{ Spec.kind3[F](${ kind[F] }) }

  private def kind[F <: AnyKind: Type](using Quotes): Expr[List[Spec.Ctor]] = {
    val encoder = new Encoder(Type.show[F])
    encoder.kind[F]
    encoder.table
  }

  private final class Encoder(requested: String)(using Quotes) {
    import quotes.reflect.*

    private val indices = mutable.Map.empty[Symbol, Int]
    private val specs = mutable.ArrayBuffer.empty[Spec.Ctor]

    def table: Expr[List[Spec.Ctor]] = Expr.ofList(specs.toList.map(lift))

    def root[T: Type]: Spec.T = tpe(TypeRepr.of[T], Nil)

    // The holes need no check of their own: the compiler refuses a constructor whose parameters are bounded or
    // higher-kinded as an argument for `F[_]`, `F[_, _]` and `F[_, _, _]`
    def kind[F <: AnyKind: Type]: Unit = {
      // F arrives eta-expanded, as `[A] =>> Option[A]`, even when it names a class
      val sym = TypeRepr.of[F].dealias match {
        case lambda @ TypeLambda(_, _, AppliedType(tycon: TypeRef, args))
            if tycon.typeSymbol.isClassDef && args.length == lambda.paramNames.length &&
              args.indices.forall(i => args(i) == lambda.param(i)) =>
          tycon.typeSymbol
        case _ => fail("only a class or trait whose parameters are the holes, in order, can be a Kind")
      }
      val _ = ctor(sym)
    }

    // Symbol.info would say it directly, but it is experimental
    private def bounds(param: Symbol): TypeRepr = param.owner.typeRef.memberType(param)

    private def typeParams(sym: Symbol): List[Symbol] = sym.declaredTypes.filter(_.isTypeParam)

    private var where: Option[String] = None

    private def within[A](place: String)(body: => A): A = {
      val outer = where
      where = Some(where.getOrElse(place))
      try body
      finally where = outer
    }

    private def fail(reason: String): Nothing =
      report.errorAndAbort(
        s"$requested is not supported by h8io.reflect: $reason${where.fold("")(place => s", in $place")}")

    private def ctor(sym: Symbol): Int =
      indices.getOrElse(
        sym, {
          val index = specs.length
          indices(sym) = index
          specs += null
          specs(index) = describe(sym)
          index
        })

    private def describe(cls: Symbol): Spec.Ctor = {
      if (!static(cls)) fail(s"${cls.fullName} is local or path-dependent")
      val params = typeParams(cls)
      val self = if (params.isEmpty) cls.typeRef else cls.typeRef.appliedTo(params.map(_.typeRef))
      val variances = params.map { p =>
        if (p.flags.is(Flags.Covariant)) Variance.Covariant
        else if (p.flags.is(Flags.Contravariant)) Variance.Contravariant
        else Variance.Invariant
      }
      val bases = cls.typeRef.baseClasses.tail.filterNot(typeParams(_).exists(isHigherKinded)).map { base =>
        val baseType = self.baseType(base)
        within(s"the base type ${baseType.show} of ${cls.fullName}") {
          Spec.Base(base.fullName, baseType.typeArgs.map(tpe(_, params)))
        }
      }
      Spec.Ctor(cls.fullName, variances, TypeRepr.of[Null] <:< self, bases)
    }

    private def isHigherKinded(param: Symbol): Boolean =
      bounds(param) match {
        case TypeBounds(_, _: TypeLambda) => true
        case _ => false
      }

    private def static(sym: Symbol): Boolean =
      sym.owner.isPackageDef || (sym.owner.flags.is(Flags.Module) && static(sym.owner))

    private def tpe(t: TypeRepr, params: List[Symbol]): Spec.T =
      t.dealias match {
        case _: OrType => fail(s"union ${t.show}")
        case _: AndType => fail(s"intersection ${t.show}")
        case _: Refinement => fail(s"refinement ${t.show}")
        case AppliedType(tycon: TypeRef, args) if tycon.typeSymbol.isClassDef =>
          Spec.T.Ref(ctor(tycon.typeSymbol), args.map(tpe(_, params)))
        case ref: TypeRef =>
          val sym = ref.typeSymbol
          if (sym == defn.NothingClass) Spec.T.Nothing
          else if (sym == defn.NullClass) Spec.T.Null
          else if (params.contains(sym)) Spec.T.Param(params.indexOf(sym))
          else if (sym.isClassDef && typeParams(sym).isEmpty) Spec.T.Ref(ctor(sym), Nil)
          else fail(
            s"abstract, opaque or higher-kinded type ${t.show}; generic code builds types with Kind1, Kind2 and Kind3")
        case ref: TermRef if ref.termSymbol.flags.is(Flags.Module) => Spec.T.Ref(ctor(ref.termSymbol.moduleClass), Nil)
        case _: TermRef | _: ConstantType | _: ThisType => fail(s"singleton type ${t.show}")
        case _: TypeLambda => fail(s"higher-kinded ${t.show}")
        case _ => fail(s"${t.show} is outside the supported subset")
      }
  }

  private def lift(variance: Variance)(using Quotes): Expr[Variance] =
    variance match {
      case Variance.Covariant => '{ Variance.Covariant }
      case Variance.Contravariant => '{ Variance.Contravariant }
      case Variance.Invariant => '{ Variance.Invariant }
    }

  private def lift(t: Spec.T)(using Quotes): Expr[Spec.T] =
    t match {
      case Spec.T.Nothing => '{ Spec.T.Nothing }
      case Spec.T.Null => '{ Spec.T.Null }
      case Spec.T.Param(index) => '{ Spec.T.Param(${ Expr(index) }) }
      case Spec.T.Ref(ctor, args) => '{ Spec.T.Ref(${ Expr(ctor) }, ${ Expr.ofList(args.map(lift)) }) }
    }

  private def lift(base: Spec.Base)(using Quotes): Expr[Spec.Base] =
    '{ Spec.Base(${ Expr(base.name) }, ${ Expr.ofList(base.args.map(lift)) }) }

  private def lift(ctor: Spec.Ctor)(using Quotes): Expr[Spec.Ctor] =
    '{
      Spec.Ctor(
        ${ Expr(ctor.name) },
        ${ Expr.ofList(ctor.variances.map(lift)) },
        ${ Expr(ctor.nullable) },
        ${ Expr.ofList(ctor.bases.map(lift)) })
    }
}
