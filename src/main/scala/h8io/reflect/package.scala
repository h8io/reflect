package h8io

package object reflect {
  implicit def tpe[T](implicit typeTag: scala.reflect.runtime.universe.TypeTag[T]): Type[T] =
    new Type[T] {
      override private[reflect] val tag: scala.reflect.runtime.universe.TypeTag[T] = typeTag
    }
}
