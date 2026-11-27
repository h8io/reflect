package h8io.reflect

import h8io.reflect.test.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Scala3Test extends AnyFlatSpec with Matchers {
  "Matchable" should "be above AnyVal and AnyRef but not above universal traits" in {
    Type[Int] <:< Type[Matchable] shouldBe true
    assertCompiles("implicitly[Int <:< Matchable]")
    Type[String] <:< Type[Matchable] shouldBe true
    assertCompiles("implicitly[String <:< Matchable]")
    Type[Null] <:< Type[Matchable] shouldBe true
    assertCompiles("implicitly[Null <:< Matchable]")
    TypeUniversalTrait <:< Type[Matchable] shouldBe false
    assertTypeError("implicitly[UniversalTrait <:< Matchable]")
    Type[Matchable] <:< Type[Any] shouldBe true
    Type[Any] <:< Type[Matchable] shouldBe false
  }

  "Unions and intersections" should "be compile errors, in base types too" in {
    assertTypeError("Type[Int | String]")
    assertTypeError("Type[RefTrait & Serializable]")
    assertTypeError("Type[CBoxBoth]")
  }
}
