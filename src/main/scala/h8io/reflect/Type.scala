package h8io.reflect

sealed trait Variant[T] {
  def accepts(tp: Type[?]): Boolean

  def unary_+ : Variant[T] = this

  def unary_- : Variant[T]
}

trait Type[T] extends Variant[T] {
  private[reflect] val tag: scala.reflect.runtime.universe.TypeTag[T]

  final def <:<(that: Type[?]): Boolean = tag.tpe.dealias <:< that.tag.tpe.dealias

  @inline final def =:=(that: Type[?]): Boolean = tag.tpe.dealias =:= that.tag.tpe.dealias

  final def accepts(that: Type[?]): Boolean = this =:= that

  override final def unary_+ : Covariant[T] = Covariant(this)

  final def unary_- : Contravariant[T] = Contravariant(this)

  override final def toString: String = tag.tpe.dealias.toString
}

object Invariant {
  def apply[T](tp: Type[T]): Variant[T] = tp

  def unapply[T](tp: Type[T]): Option[Type[T]] = Some(tp)
}

final case class Covariant[T](tp: Type[T]) extends Variant[T] {
  def accepts(that: Type[?]): Boolean = that <:< tp

  def unary_- : Contravariant[T] = Contravariant(tp)

  override def toString: String = s"+${tp}"
}

final case class Contravariant[T](tp: Type[T]) extends Variant[T] {
  def accepts(that: Type[?]): Boolean = tp <:< that

  def unary_- : Covariant[T] = Covariant(tp)

  override def toString: String = s"-${tp}"
}
