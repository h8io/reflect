package h8io.reflect.internal

import scala.collection.mutable
import scala.reflect.macros.blackbox

/** The Scala 2 macros behind `Type`, `Kind1`, `Kind2` and `Kind3`. Not part of the API. */
final class Macros(val c: blackbox.Context) {
  import c.universe.*

  private val S = q"_root_.h8io.reflect.internal.Spec"

  def tpe[T](implicit t: c.WeakTypeTag[T]): Tree = {
    val encoder = new Encoder(t.tpe.toString)
    val root = encoder.tpe(t.tpe, Nil)
    q"$S.tpe[${t.tpe}](${encoder.table}, ${lift(root)})"
  }

  def kind1[F[_]]: Tree = kind(TermName("kind1"))
  def kind2[F[_, _]]: Tree = kind(TermName("kind2"))
  def kind3[F[_, _, _]]: Tree = kind(TermName("kind3"))

  // The holes need no check of their own: the compiler refuses a constructor whose parameters are bounded or
  // higher-kinded as an argument for `F[_]`, `F[_, _]` and `F[_, _, _]`
  private def kind(factory: TermName): Tree = {
    val tycon = c.macroApplication match {
      case TypeApply(_, List(tpt)) => tpt.tpe
      case other => fail(other.toString, "not a type constructor application")
    }
    val name = tycon.toString
    val sym = tycon.etaExpand match {
      case PolyType(params, result) =>
        result.dealias match {
          case TypeRef(_, sym, args) if sym.isClass && args.map(_.typeSymbol) == params => sym
          case _ => fail(name, "only a class or trait whose parameters are the holes, in order, can be a Kind")
        }
      case _ => fail(name, "not a type constructor")
    }
    val encoder = new Encoder(name)
    val _ = encoder.ctor(sym)
    q"$S.$factory[$tycon](${encoder.table})"
  }

  private def fail(tpe: String, reason: String): Nothing =
    c.abort(c.enclosingPosition, s"$tpe is not supported by h8io.reflect: $reason")

  private final class Encoder(requested: String) {
    private var where: Option[String] = None

    private def within[A](place: String)(body: => A): A = {
      val outer = where
      where = Some(where.getOrElse(place))
      try body
      finally where = outer
    }

    private def fail(reason: String): Nothing = Macros.this.fail(requested, reason + where.fold("")(p => s", in $p"))
    private val indices = mutable.Map.empty[Symbol, Int]
    private val specs = mutable.ArrayBuffer.empty[Spec.Ctor]

    def table: Tree = q"_root_.scala.List(..${specs.toList.map(lift)})"

    def ctor(sym: Symbol): Int =
      indices.getOrElse(
        sym, {
          val index = specs.length
          indices(sym) = index
          specs += null
          specs(index) = describe(sym.asClass)
          index
        })

    private def describe(cls: ClassSymbol): Spec.Ctor = {
      if (!static(cls)) fail(s"$cls is local or path-dependent")
      val params = cls.typeParams
      val self = cls.toType
      val variances = params.map { p =>
        if (p.asType.isCovariant) Variance.Covariant
        else if (p.asType.isContravariant) Variance.Contravariant
        else Variance.Invariant
      }
      val bases = cls.baseClasses.tail.filterNot(_.asType.typeParams.exists(_.asType.typeParams.nonEmpty)).map {
        base =>
          val baseType = self.baseType(base)
          within(s"the base type $baseType of ${cls.fullName}") {
            Spec.Base(base.fullName, baseType.typeArgs.map(tpe(_, params)))
          }
      }
      // An object and its companion class share the full name
      val name = if (cls.isModuleClass) s"${cls.fullName}.type" else cls.fullName
      Spec.Ctor(name, variances, definitions.NullTpe <:< self, bases)
    }

    private def static(sym: Symbol): Boolean =
      sym.owner.isPackageClass || (sym.owner.isModuleClass && static(sym.owner))

    def tpe(t: Type, params: List[Symbol]): Spec.T =
      t.dealias match {
        case TypeRef(_, sym, Nil) if sym == definitions.NothingClass => Spec.T.Nothing
        case TypeRef(_, sym, Nil) if sym == definitions.NullClass => Spec.T.Null
        case TypeRef(_, sym, Nil) if params.contains(sym) => Spec.T.Param(params.indexOf(sym))
        case SingleType(_, sym) if sym.isModule => Spec.T.Ref(ctor(sym.asModule.moduleClass), Nil)
        case TypeRef(_, sym, args) if sym.isClass =>
          if (args.length != sym.asClass.typeParams.length) fail(s"$t is higher-kinded or raw")
          Spec.T.Ref(ctor(sym), args.map(tpe(_, params)))
        case TypeRef(_, sym, Nil) if sym.asType.typeParams.nonEmpty => fail(s"$t is higher-kinded")
        case TypeRef(_, sym, _) if sym.isType =>
          fail(s"abstract type $t; generic code builds types with Kind1, Kind2 and Kind3")
        case _: RefinedType => fail(s"refinement or intersection $t")
        case _: SingleType | _: ConstantType | _: ThisType => fail(s"singleton type $t")
        case _ => fail(s"$t is outside the supported subset")
      }
  }

  private def lift(variance: Variance): Tree =
    variance match {
      case Variance.Covariant => q"_root_.h8io.reflect.internal.Variance.Covariant"
      case Variance.Contravariant => q"_root_.h8io.reflect.internal.Variance.Contravariant"
      case Variance.Invariant => q"_root_.h8io.reflect.internal.Variance.Invariant"
    }

  private def lift(t: Spec.T): Tree =
    t match {
      case Spec.T.Nothing => q"$S.T.Nothing"
      case Spec.T.Null => q"$S.T.Null"
      case Spec.T.Param(index) => q"$S.T.Param($index)"
      case Spec.T.Ref(ctor, args) => q"$S.T.Ref($ctor, _root_.scala.List(..${args.map(lift)}))"
    }

  private def lift(base: Spec.Base): Tree = q"$S.Base(${base.name}, _root_.scala.List(..${base.args.map(lift)}))"

  private def lift(ctor: Spec.Ctor): Tree =
    q"$S.Ctor(${ctor.name}, _root_.scala.List(..${ctor.variances.map(lift)}), ${ctor.nullable}, _root_.scala.List(..${ctor.bases.map(lift)}))"
}
