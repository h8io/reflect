package h8io.reflect

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeAnyTest extends AnyFlatSpec with Matchers with Inside {
  import types.*

  "implicit value" should "has a correct type" in {
    implicitly[Type[Any]] should be theSameInstanceAs TypeAny
  }

  "toString" should "produce a correct string representation" in { TypeAny.toString shouldBe "Any" }

  "<:<" should "be reflexive" in { TypeAny <:< TypeAny shouldBe true }

  it should "return false for primitive types" in {
    for {
      primitive <- Primitives
    } TypeAny <:< primitive shouldBe false
  }

  it should "return false for AnyVal" in { TypeAny <:< implicitly[Type[AnyVal]] shouldBe false }

  it should "return false for AnyRef" in { TypeAny <:< implicitly[Type[AnyRef]] shouldBe false }

  it should "return false for Nothing" in { TypeAny <:< implicitly[Type[Nothing]] shouldBe false }

  it should "return false for Null" in { TypeAny <:< implicitly[Type[Null]] shouldBe false }

  it should "return false for a reference trait" in { TypeAny <:< TypeRefTrait shouldBe false }

  it should "return false for an universal trait" in { TypeAny <:< TypeUniversalTrait shouldBe false }

  it should "return false for a value class" in { TypeAny <:< TypeValClass shouldBe false }

  "=:=" should "be reflexive" in { TypeAny =:= TypeAny shouldBe true }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeAny =:= primitive shouldBe false
  }

  it should "return false for AnyVal" in { TypeAny =:= implicitly[Type[AnyVal]] shouldBe false }

  it should "return false for AnyRef" in { TypeAny =:= implicitly[Type[AnyRef]] shouldBe false }

  it should "return false for Nothing" in { TypeAny =:= implicitly[Type[Nothing]] shouldBe false }

  it should "return false for Null" in { TypeAny =:= implicitly[Type[Null]] shouldBe false }

  it should "return false for a reference trait" in { TypeAny =:= TypeRefTrait shouldBe false }

  it should "return false for an universal trait" in { TypeAny =:= TypeUniversalTrait shouldBe false }

  it should "return false for a value class" in { TypeAny =:= TypeValClass shouldBe false }
}
