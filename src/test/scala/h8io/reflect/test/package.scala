package h8io.reflect

package object test {
  val Primitives: List[Primitive[?, ?]] =
    List(TypeBoolean, TypeByte, TypeShort, TypeInt, TypeLong, TypeFloat, TypeDouble, TypeChar, TypeUnit)

  val TypeUniversalTrait: Type[UniversalTrait] = implicitly

  val TypeUniversalParentTrait: Type[UniversalParentTrait] = implicitly

  val TypeRefTrait: Type[RefTrait] = implicitly

  val TypeValClass: Type[ValClass] = implicitly
}
