package h8io.reflect

object types {
  val Primitives: List[Primitive[?, ?]] =
    List(TypeBoolean, TypeByte, TypeShort, TypeInt, TypeLong, TypeFloat, TypeDouble, TypeChar, TypeUnit)

  trait UniversalTrait extends Any

  val TypeUniversalTrait: Type[UniversalTrait] = implicitly[Type[UniversalTrait]]

  trait RefTrait extends UniversalTrait

  val TypeRefTrait: Type[RefTrait] = implicitly[Type[RefTrait]]

  final class ValClass(val value: Int) extends AnyVal with UniversalTrait

  val TypeValClass: Type[ValClass] = implicitly[Type[ValClass]]
}
