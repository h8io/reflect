package h8io.reflect

import h8io.reflect.internal.{Ctor, Repr, Variance}

/** A type constructor whose holes take `Type`s: builds types with `apply` and takes them apart with `unapply`. */
sealed abstract class Kind private[reflect] (ctor: Ctor) {
  protected final def build[T](args: Type[?]*): Type[T] = new Type(Repr.Applied(ctor, args.map(_.repr).toList))

  /** The arguments of the base type of `t` whose class is this constructor. For `Nothing`, and for `Null` when the
    * constructor admits it, the arguments that make the applied constructor least.
    */
  protected final def args(t: Type[?]): Option[List[Type[?]]] =
    (t.repr match {
      case Repr.Nothing => Some(least)
      case Repr.Null => if (ctor.nullable) Some(least) else None
      case Repr.Applied(that, args) => that.lift(args, ctor.name)
    }).map(_.map(new Type[Any](_)))

  private def least: List[Repr] =
    ctor.variances.map {
      case Variance.Contravariant => Repr.Applied(Ctor.Any, Nil)
      case Variance.Covariant | Variance.Invariant => Repr.Nothing
    }

  override def toString: String = ctor.name
}

final class Kind1[F[_]] private[reflect] (ctor: Ctor) extends Kind(ctor) {
  def apply[A](a: Type[A]): Type[F[A]] = build(a)
  def unapply(t: Type[?]): Option[Type[?]] = args(t).map(_.head)
}

object Kind1 extends Kind1Macros {
  def apply[F[_]](implicit k: Kind1[F]): Kind1[F] = k
}

final class Kind2[F[_, _]] private[reflect] (ctor: Ctor) extends Kind(ctor) {
  def apply[A, B](a: Type[A], b: Type[B]): Type[F[A, B]] = build(a, b)
  def unapply(t: Type[?]): Option[(Type[?], Type[?])] = args(t).map(args => (args.head, args(1)))
}

object Kind2 extends Kind2Macros {
  def apply[F[_, _]](implicit k: Kind2[F]): Kind2[F] = k
}

final class Kind3[F[_, _, _]] private[reflect] (ctor: Ctor) extends Kind(ctor) {
  def apply[A, B, C](a: Type[A], b: Type[B], c: Type[C]): Type[F[A, B, C]] = build(a, b, c)
  def unapply(t: Type[?]): Option[(Type[?], Type[?], Type[?])] = args(t).map(args => (args.head, args(1), args(2)))
}

object Kind3 extends Kind3Macros {
  def apply[F[_, _, _]](implicit k: Kind3[F]): Kind3[F] = k
}
