package h8io.reflect

sealed trait Variant[T] {
  val tp: Type[T]

  final def unary_~ : Invariant[T] = Invariant(tp)

  final def unary_+ : Variant[T] = this

  def unary_- : Variant[T]

  def accepts(tp: Type[?]): Boolean
}

final case class Invariant[T](tp: Type[T]) extends Variant[T] {
  def unary_- : Invariant[T] = this

  def accepts(that: Type[?]): Boolean = tp =:= that

  override def toString: String = tp.toInvariantString
}

final case class Covariant[T](tp: Type[T]) extends Variant[T] {
  def unary_- : Contravariant[T] = Contravariant(tp)

  def accepts(that: Type[?]): Boolean = that <:< tp

  override def toString: String = s"+$tp"
}

final case class Contravariant[T](tp: Type[T]) extends Variant[T] {
  def unary_- : Covariant[T] = Covariant(tp)

  def accepts(that: Type[?]): Boolean = tp <:< that

  override def toString: String = s"-${tp.toContravariantString}"
}
