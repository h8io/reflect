package h8io.reflect

package object test {
  val Primitives: List[Type[?]] =
    List(
      Type[Boolean], Type[Byte], Type[Short], Type[Int], Type[Long], Type[Float], Type[Double], Type[Char], Type[Unit])

  val TypeUniversalTrait: Type[UniversalTrait] = implicitly

  val TypeUniversalParentTrait: Type[UniversalParentTrait] = implicitly

  val TypeRefTrait: Type[RefTrait] = implicitly

  val TypeValClass: Type[ValClass] = implicitly
}
