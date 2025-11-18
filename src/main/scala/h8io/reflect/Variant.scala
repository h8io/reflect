package h8io.reflect

sealed trait Variant[T] {
  def accepts(tp: Type[?]): Boolean
}

final case class Invariant[T](tp: Type[T]) extends Variant[T] {
  def accepts(that: Type[?]): Boolean = tp =:= that

  override def toString: String = tp.toString
}

final case class Covariant[T](tp: Type[T]) extends Variant[T] {
  def accepts(that: Type[?]): Boolean = that <:< tp

  override def toString: String = s"+$tp"
}

final case class Contravariant[T](tp: Type[T]) extends Variant[T] {
  def accepts(that: Type[?]): Boolean = tp <:< that

  override def toString: String = s"-$tp"
}
