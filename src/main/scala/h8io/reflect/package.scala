package h8io

import scala.reflect.runtime.universe

package object reflect {
  implicit def typeOf[T](implicit tag: universe.WeakTypeTag[T]): Type[T] = new Type(tag.tpe.dealias)
}
