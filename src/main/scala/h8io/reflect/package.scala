package h8io

package object reflect {
  type Type[T] = Invariant

  implicit def tpe[T](implicit tag: scala.reflect.runtime.universe.TypeTag[T]): Type[T] = new Invariant(tag.tpe.dealias)
}
