package h8io

package object reflect {
  implicit def typeOf[T](implicit tag: scala.reflect.runtime.universe.TypeTag[T]): Type[T] = new Type(tag.tpe.dealias)
}
