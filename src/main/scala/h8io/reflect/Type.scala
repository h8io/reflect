package h8io.reflect

sealed trait Variant[T] {
  def accepts(tp: Type[?]): Boolean

  def unary_+ : Variant[T] = this

  def unary_- : Variant[T]
}

final case class Type[T](private val tag: scala.reflect.runtime.universe.TypeTag[T]) extends Variant[T] {
  def <:<(that: Type[?]): Boolean = tag.tpe.dealias <:< that.tag.tpe.dealias

  @inline def =:=(that: Type[?]): Boolean = tag.tpe.dealias =:= that.tag.tpe.dealias

  def accepts(that: Type[?]): Boolean = this =:= that

  override def unary_+ : Covariant[T] = Covariant(this)

  def unary_- : Contravariant[T] = Contravariant(this)

  override def toString: String = tag.tpe.dealias.toString
}

object Invariant {
  def apply[T](tp: Type[T]): Invariant[T] = tp

  def unapply[T](tp: Type[T]): Option[Type[T]] = Some(tp)
}

final case class Covariant[T](tp: Type[T]) extends Variant[T] {
  def accepts(that: Type[?]): Boolean = that <:< tp

  def unary_- : Contravariant[T] = Contravariant(tp)

  override def toString: String = s"+$tp"
}

final case class Contravariant[T](tp: Type[T]) extends Variant[T] {
  def accepts(that: Type[?]): Boolean = tp <:< that

  def unary_- : Covariant[T] = Covariant(tp)

  override def toString: String = s"-$tp"
}
