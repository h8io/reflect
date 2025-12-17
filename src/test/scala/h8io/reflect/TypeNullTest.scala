package h8io.reflect

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeNullTest extends AnyFlatSpec with Matchers with Inside {
  import types.*

  "implicit value" should "has a correct type" in {
    implicitly[Type[Null]] should be theSameInstanceAs TypeNull
  }

  "toString" should "produce a correct string representation" in {
    TypeNull.toString shouldBe "Null"
  }

  "<:<" should "be reflexive" in {
    TypeNull <:< TypeNull shouldBe true
  }

  it should "return false for primitive types" in {
    for {
      primitive <- Primitives
    } TypeNull <:< primitive shouldBe false
  }

  it should "return true for Any" in {
    TypeNull <:< implicitly[Type[Any]] shouldBe true
  }

  it should "return false for AnyVal" in {
    TypeNull <:< implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return true for AnyRef" in {
    TypeNull <:< implicitly[Type[AnyRef]] shouldBe true
  }

  it should "return false for Nothing" in {
    TypeNull <:< implicitly[Type[Nothing]] shouldBe false
  }

  it should "return true for a reference trait" in {
    TypeNull <:< TypeRefTrait shouldBe true
  }

  it should "return false for an universal trait" in {
    TypeNull <:< TypeUniversalTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeNull <:< TypeValClass shouldBe false
  }

  "=:=" should "be reflexive" in {
    TypeNull =:= TypeNull shouldBe true
  }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeNull =:= primitive shouldBe false
  }

  it should "return false for Any" in {
    TypeNull =:= implicitly[Type[Any]] shouldBe false
  }

  it should "return false for AnyVal" in {
    TypeNull =:= implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for AnyRef" in {
    TypeNull =:= implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeNull =:= implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeNull =:= TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeNull =:= TypeUniversalTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeNull =:= TypeValClass shouldBe false
  }
}
