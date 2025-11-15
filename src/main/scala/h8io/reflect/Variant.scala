package h8io.reflect

import scala.reflect.runtime.universe

sealed trait Variant {
  def accepts(tp: Type[?]): Boolean

  def unary_+ : Variant = this

  def unary_- : Variant
}

final class Invariant private[reflect] (private val tpe: universe.Type) extends Variant {
  val parameters: Iterable[Variant] =
    (tpe.typeConstructor.typeParams.iterator zip tpe.typeArgs.iterator).map { case (symbol, arg) =>
      val tp = new Invariant(arg)
      if (symbol.asType.isCovariant) Covariant(tp)
      else if (symbol.asType.isContravariant) Contravariant(tp)
      else tp
    }.toList

  def <:<(that: Invariant): Boolean = tpe <:< that.tpe

  @inline def =:=(that: Invariant): Boolean = tpe =:= that.tpe

  def accepts(that: Type[?]): Boolean = this =:= that

  override def unary_+ : Covariant = Covariant(this)

  def unary_- : Contravariant = Contravariant(this)

  override def toString: String = tpe.toString

  override def hashCode(): Int = tpe.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: Invariant => this =:= that
    case _ => false
  }
}

object Invariant {
  def apply(tp: Type[?]): Invariant = tp

  def unapply(tp: Invariant): Option[Type[?]] = Some(tp)
}

final case class Covariant(tp: Invariant) extends Variant {
  def accepts(that: Type[?]): Boolean = that <:< tp

  def unary_- : Contravariant = Contravariant(tp)

  override def toString: String = s"+$tp"
}

final case class Contravariant(tp: Invariant) extends Variant {
  def accepts(that: Type[?]): Boolean = tp <:< that

  def unary_- : Covariant = Covariant(tp)

  override def toString: String = s"-$tp"
}
