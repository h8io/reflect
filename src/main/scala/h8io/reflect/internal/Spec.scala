package h8io.reflect.internal

import h8io.reflect.{Kind1, Kind2, Kind3, Type}

/** What the macros emit: a table of classes and a type over it. Public only because the expanded code lives at the call
  * site; not part of the API.
  *
  * Classes refer to each other by their index in the table, so that a class can occur in its own base types (`String <:
  * Comparable[String]`).
  */
object Spec {
  sealed abstract class T

  object T {
    case object Nothing extends T
    case object Null extends T

    /** The class's own type parameter at `index`; only inside base types. */
    final case class Param(index: Int) extends T

    final case class Ref(ctor: Int, args: List[T]) extends T
  }

  final case class Base(name: String, args: List[T])

  final case class Ctor(name: String, variances: List[Variance], nullable: Boolean, bases: List[Base])

  def tpe[A](table: List[Ctor], root: T): Type[A] = new Type(new Table(table).repr(root))

  def kind1[F[_]](table: List[Ctor]): Kind1[F] = new Kind1(new Table(table).ctors(0))

  def kind2[F[_, _]](table: List[Ctor]): Kind2[F] = new Kind2(new Table(table).ctors(0))

  def kind3[F[_, _, _]](table: List[Ctor]): Kind3[F] = new Kind3(new Table(table).ctors(0))

  private[internal] final class Table(specs: List[Ctor]) {
    lazy val ctors: Vector[h8io.reflect.internal.Ctor] = specs.toVector.map { spec =>
      new h8io.reflect.internal.Ctor(
        spec.name,
        spec.variances,
        spec.nullable,
        spec.bases.map(base => base.name -> base.args.map(tpl)).toMap)
    }

    def repr(t: T): Repr =
      t match {
        case T.Nothing => Repr.Nothing
        case T.Null => Repr.Null
        case T.Ref(ctor, args) => Repr.Applied(ctors(ctor), args.map(repr))
        case T.Param(index) => throw new IllegalArgumentException(s"Type parameter #$index outside a base type")
      }

    private def tpl(t: T): Tpl =
      t match {
        case T.Nothing => Tpl.Nothing
        case T.Null => Tpl.Null
        case T.Ref(ctor, args) => Tpl.Applied(ctors(ctor), args.map(tpl))
        case T.Param(index) => Tpl.Param(index)
      }
  }
}
