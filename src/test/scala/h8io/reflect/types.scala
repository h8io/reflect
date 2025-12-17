package h8io.reflect

object types {
  val Primitives: List[Primitive[?, ?]] =
    List(TypeBoolean, TypeByte, TypeShort, TypeInt, TypeLong, TypeFloat, TypeDouble, TypeChar, TypeUnit)

  trait RefTrait

  val TypeRefTrait: Type[RefTrait] = implicitly[Type[RefTrait]]

  trait UniversalTrait extends Any

  val TypeUniversalTrait: Type[UniversalTrait] = implicitly[Type[UniversalTrait]]

  final class ValClass(val value: Int) extends AnyVal with UniversalTrait

  val TypeValClass: Type[ValClass] = implicitly[Type[ValClass]]
}
