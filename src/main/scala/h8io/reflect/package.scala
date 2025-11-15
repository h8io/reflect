package h8io

package object reflect {
  implicit def tpe[T](implicit tag: scala.reflect.runtime.universe.TypeTag[T]): Type[T] = Type(tag)

  type Invariant[T] = Type[T]
}
