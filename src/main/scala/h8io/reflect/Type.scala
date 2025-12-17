package h8io.reflect

import izumi.reflect.macrortti.{LWeakTag, LightTypeTag}

import scala.runtime.BoxedUnit

sealed trait Type[T] {
  def <:<(that: Type[?]): Boolean
  def =:=(that: Type[?]): Boolean
}

private case object TypeAny extends Type[Any] {
  def <:<(that: Type[?]): Boolean = that eq this
  def =:=(that: Type[?]): Boolean = that eq this

  override def toString: String = "Any"
}

private case object TypeAnyVal extends Type[AnyVal] {
  def <:<(that: Type[?]): Boolean = (that eq TypeAny) || (that eq this)
  def =:=(that: Type[?]): Boolean = that eq this

  override def toString: String = "AnyVal"
}

private case object TypeAnyRef extends Type[AnyRef] {
  def <:<(that: Type[?]): Boolean = (that eq TypeAny) || (that eq this)
  def =:=(that: Type[?]): Boolean = that eq this

  override def toString: String = "AnyRef"
}

private case object TypeNothing extends Type[Nothing] {
  def <:<(that: Type[?]): Boolean = true
  def =:=(that: Type[?]): Boolean = that eq this

  override def toString: String = "Nothing"
}

private case object TypeNull extends Type[Null] {
  def <:<(that: Type[?]): Boolean =
    that match {
      case TypeNull | TypeRef(_) | TypeAnyRef | TypeAny => true
      case _ => false
    }
  def =:=(that: Type[?]): Boolean = that eq this

  override def toString: String = "Null"
}

sealed abstract class Primitive[T <: AnyVal, B <: AnyRef](override val toString: String)(implicit val boxed: Type[B])
    extends Type[T] {
  def <:<(that: Type[?]): Boolean = (that eq TypeAny) || (that eq TypeAnyVal) || (that eq this)
  def =:=(that: Type[?]): Boolean = that eq this
}

private case object TypeBoolean extends Primitive[Boolean, java.lang.Boolean]("Boolean")
private case object TypeByte extends Primitive[Byte, java.lang.Byte]("Byte")
private case object TypeShort extends Primitive[Short, java.lang.Short]("Short")
private case object TypeInt extends Primitive[Int, java.lang.Integer]("Int")
private case object TypeLong extends Primitive[Long, java.lang.Long]("Long")
private case object TypeFloat extends Primitive[Float, java.lang.Float]("Float")
private case object TypeDouble extends Primitive[Double, java.lang.Double]("Double")
private case object TypeChar extends Primitive[Char, java.lang.Character]("Char")
private case object TypeUnit extends Primitive[Unit, BoxedUnit]("Unit")

private final case class TypeVal[T <: AnyVal](tag: LightTypeTag) extends Type[T] {
  def <:<(that: Type[?]): Boolean =
    that match {
      case TypeAny | TypeAnyVal => true
      case TypeVal(thatTag) => tag <:< thatTag
      case TypeAnyLike(thatTag) => tag <:< thatTag
      case _ => false
    }

  def =:=(that: Type[?]): Boolean =
    that match {
      case TypeVal(thatTag) => tag =:= thatTag
      case _ => false
    }

  override def toString: String = tag.scalaStyledRepr
}

private final case class TypeRef[T <: AnyRef](tag: LightTypeTag) extends Type[T] {
  def <:<(that: Type[?]): Boolean =
    that match {
      case TypeAny | TypeAnyRef => true
      case TypeRef(thatTag) => tag <:< thatTag
      case TypeAnyLike(thatTag) => tag <:< thatTag
      case _ => false
    }

  def =:=(that: Type[?]): Boolean =
    that match {
      case TypeRef(thatTag) => tag =:= thatTag
      case _ => false
    }

  override def toString: String = tag.scalaStyledRepr
}

private final case class TypeAnyLike[T](tag: LightTypeTag) extends Type[T] {
  def <:<(that: Type[?]): Boolean =
    that match {
      case TypeAny => true
      case TypeAnyLike(thatTag) => tag <:< thatTag
      case _ => false
    }

  def =:=(that: Type[?]): Boolean =
    that match {
      case TypeAnyLike(thatTag) => tag =:= thatTag
      case _ => false
    }

  override def toString: String = tag.scalaStyledRepr
}

object Type {
  implicit def typeAny: Type[Any] = TypeAny
  implicit def typeAnyVal: Type[AnyVal] = TypeAnyVal
  implicit def typeAnyRef: Type[AnyRef] = TypeAnyRef
  implicit def typeNothing: Type[Nothing] = TypeNothing
  implicit def typeNull: Type[Null] = TypeNull

  implicit def typeBoolean: Type[Boolean] = TypeBoolean
  implicit def typeByte: Type[Byte] = TypeByte
  implicit def typeShort: Type[Short] = TypeShort
  implicit def typeInt: Type[Int] = TypeInt
  implicit def typeLong: Type[Long] = TypeLong
  implicit def typeFloat: Type[Float] = TypeFloat
  implicit def typeDouble: Type[Double] = TypeDouble
  implicit def typeChar: Type[Char] = TypeChar
  implicit def typeUnit: Type[Unit] = TypeUnit

  implicit def typeVal[T <: AnyVal](implicit tag: LWeakTag[T]): Type[T] = TypeVal(tag.tag)
  implicit def typeRef[T <: AnyRef](implicit tag: LWeakTag[T]): Type[T] = TypeRef(tag.tag)

  implicit def typeAnyLike[T](implicit tag: LWeakTag[T]): Type[T] = TypeAnyLike(tag.tag)
}
