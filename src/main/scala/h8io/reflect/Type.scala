package h8io.reflect

import scala.reflect.runtime.universe

final class Type[T] private[reflect] (private[reflect] val tpe: universe.Type) {
  val parameters: Iterable[Variant[?]] =
    (tpe.typeConstructor.typeParams.iterator zip tpe.typeArgs.iterator).map { case (symbol, arg) =>
      val tp = new Type[Any](arg)
      if (symbol.asType.isCovariant) Covariant(tp)
      else if (symbol.asType.isContravariant) Contravariant(tp)
      else Invariant(tp)
    }.toList

  def <:<(that: Type[?]): Boolean = tpe <:< that.tpe

  @inline def =:=(that: Type[?]): Boolean = tpe =:= that.tpe

  def unary_~ : Invariant[T] = Invariant(this)

  def unary_+ : Covariant[T] = Covariant(this)

  def unary_- : Variant[T] = Contravariant(this)

  override def hashCode(): Int = tpe.hashCode()

  override def equals(obj: Any): Boolean =
    obj match {
      case that: Type[?] => this =:= that
      case _ => false
    }

  override def toString: String = {
    val base = universe.show(tpe.typeConstructor)
    if (parameters.isEmpty) base
    else s"$base[${parameters.mkString(", ")}]"
  }
}
