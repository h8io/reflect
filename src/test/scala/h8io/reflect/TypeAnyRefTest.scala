package h8io.reflect

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeAnyRefTest extends AnyFlatSpec with Matchers with Inside {
  import types.*

  "implicit value" should "has a correct type" in {
    implicitly[Type[AnyRef]] should be theSameInstanceAs TypeAnyRef
  }

  "toString" should "produce a correct string representation" in {
    TypeAnyRef.toString shouldBe "AnyRef"
  }

  "<:<" should "be reflexive" in {
    TypeAnyRef <:< TypeAny shouldBe true
  }

  it should "return false for primitive types" in {
    for {
      primitive <- Primitives
    } TypeAnyRef <:< primitive shouldBe false
  }

  it should "return true for Any" in {
    TypeAnyRef <:< implicitly[Type[Any]] shouldBe true
  }

  it should "return false for AnyVal" in {
    TypeAnyRef <:< implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeAnyRef <:< implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeAnyRef <:< implicitly[Type[Null]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeAnyRef <:< TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeAnyRef <:< TypeUniversalTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeAnyRef <:< TypeValClass shouldBe false
  }

  "=:=" should "be reflexive" in {
    TypeAnyRef =:= TypeAnyRef shouldBe true
  }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeAnyRef =:= primitive shouldBe false
  }

  it should "return false for Any" in {
    TypeAnyRef =:= implicitly[Type[Any]] shouldBe false
  }

  it should "return false for AnyVal" in {
    TypeAnyRef =:= implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeAnyRef =:= implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeAnyRef =:= implicitly[Type[Null]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeAnyRef =:= TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeAnyRef =:= TypeUniversalTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeAnyRef =:= TypeValClass shouldBe false
  }
}
