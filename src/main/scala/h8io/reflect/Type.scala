package h8io.reflect

import h8io.reflect.internal.Repr

/** A type known at runtime. Summoned by an implicit macro: `Type[List[Int]]`.
  *
  * `<:<` and `=:=` answer exactly what the compiler proves on the Scala version in use, that is, whether
  * `implicitly[A <:< B]` (or `=:=`) would compile.
  */
final class Type[T] private[reflect] (private[reflect] val repr: Repr) {
  def <:<(that: Type[?]): Boolean = repr <:< that.repr
  def =:=(that: Type[?]): Boolean = repr =:= that.repr

  override def toString: String = repr.toString
}

object Type extends TypeMacros {
  def apply[T](implicit t: Type[T]): Type[T] = t
}
