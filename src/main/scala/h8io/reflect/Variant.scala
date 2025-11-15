package h8io.reflect

import scala.reflect.runtime.universe

sealed trait Variant {
  def accepts(tp: Type[?]): Boolean

  def unary_+ : Variant = this

  def unary_- : Variant
}

final class Invariant private[reflect] (private val tpe: universe.Type) extends Variant {
  val parameters: Iterable[Variant] = tpe.dealias.typeParams.iterator.map { symbol =>
    val tpSymbol = symbol.asType
    val tp = new Invariant(tpSymbol.toType)
    if (tpSymbol.isCovariant) Covariant(tp)
    else if (tpSymbol.isContravariant) Contravariant(tp)
    else tp
  }.toList

  def <:<(that: Invariant): Boolean = tpe.dealias <:< that.tpe.dealias

  @inline def =:=(that: Invariant): Boolean = tpe.dealias =:= that.tpe.dealias

  def accepts(that: Type[?]): Boolean = this =:= that

  override def unary_+ : Covariant = Covariant(this)

  def unary_- : Contravariant = Contravariant(this)

  override def toString: String = tpe.dealias.toString
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
