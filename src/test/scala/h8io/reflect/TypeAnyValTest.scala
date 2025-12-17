package h8io.reflect

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeAnyValTest extends AnyFlatSpec with Matchers with Inside {
  import test.*

  "implicit value" should "has a correct type" in {
    implicitly[Type[AnyVal]] should be theSameInstanceAs TypeAnyVal
  }

  "toString" should "produce a correct string representation" in {
    TypeAnyVal.toString shouldBe "AnyVal"
  }

  "<:<" should "be reflexive" in {
    TypeAnyVal <:< TypeAnyVal shouldBe true
  }

  it should "return false for primitive types" in {
    for {
      primitive <- Primitives
    } TypeAnyVal <:< primitive shouldBe false
  }

  it should "return true for Any" in {
    TypeAnyVal <:< implicitly[Type[Any]] shouldBe true
  }

  it should "return false for AnyRef" in {
    TypeAnyVal <:< implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeAnyVal <:< implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeAnyVal <:< implicitly[Type[Null]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeAnyVal <:< TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeAnyVal <:< TypeUniversalTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeAnyVal <:< TypeValClass shouldBe false
  }

  "=:=" should "be reflexive" in {
    TypeAnyVal =:= TypeAnyVal shouldBe true
  }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeAnyVal =:= primitive shouldBe false
  }

  it should "return false for Any" in {
    TypeAnyVal =:= implicitly[Type[Any]] shouldBe false
  }

  it should "return false for AnyRef" in {
    TypeAnyVal =:= implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeAnyVal =:= implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeAnyVal =:= implicitly[Type[Null]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeAnyVal =:= TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeAnyVal =:= TypeUniversalTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeAnyVal =:= TypeValClass shouldBe false
  }
}
