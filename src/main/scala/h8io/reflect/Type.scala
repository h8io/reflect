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

sealed abstract class Primitive[T <: AnyVal, B <: AnyRef](override val toString: String)(implicit boxedTag: LWeakTag[B])
    extends Type[T] {
  def <:<(that: Type[?]): Boolean = (that eq TypeAny) || (that eq TypeAnyVal) || (that eq this)
  def =:=(that: Type[?]): Boolean = that eq this

  final val boxed: Type[B] = TypeRef(boxedTag.tag)
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
      case TypeRef(thatTag) =>
        println(s"TypeRef: tag: $tag, thatTag: $thatTag")
        tag <:< thatTag
      case TypeAnyLike(thatTag) =>
        println(s"TypeAnyLike: tag: $tag, thatTag: $thatTag")
        tag <:< thatTag
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

private[reflect] sealed trait Priority1Types {
  implicit val typeAny: Type[Any] = TypeAny
  implicit val typeAnyVal: Type[AnyVal] = TypeAnyVal
  implicit val typeAnyRef: Type[AnyRef] = TypeAnyRef
  implicit val typeNothing: Type[Nothing] = TypeNothing
  implicit val typeNull: Type[Null] = TypeNull

  implicit val typeBoolean: Type[Boolean] = TypeBoolean
  implicit val typeByte: Type[Byte] = TypeByte
  implicit val typeShort: Type[Short] = TypeShort
  implicit val typeInt: Type[Int] = TypeInt
  implicit val typeLong: Type[Long] = TypeLong
  implicit val typeFloat: Type[Float] = TypeFloat
  implicit val typeDouble: Type[Double] = TypeDouble
  implicit val typeChar: Type[Char] = TypeChar
  implicit val typeUnit: Type[Unit] = TypeUnit
}

private[reflect] sealed trait Priority2Types {
  implicit def typeVal[T <: AnyVal](implicit tag: LWeakTag[T]): Type[T] = TypeVal(tag.tag)
  implicit def typeRef[T <: AnyRef](implicit tag: LWeakTag[T]): Type[T] = TypeRef(tag.tag)
}

private[reflect] sealed trait Priority3Types {
  implicit def typeAnyLike[T](implicit tag: LWeakTag[T]): Type[T] = TypeAnyLike(tag.tag)
}

object Type extends Priority1Types with Priority2Types with Priority3Types
