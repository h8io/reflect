package h8io.reflect

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeNothingTest extends AnyFlatSpec with Matchers with Inside {
  import types.*

  "implicit value" should "has a correct type" in {
    implicitly[Type[Nothing]] should be theSameInstanceAs TypeNothing
  }

  "toString" should "produce a correct string representation" in {
    TypeNothing.toString shouldBe "Nothing"
  }

  "<:<" should "be reflexive" in {
    TypeNothing <:< TypeNothing shouldBe true
  }

  it should "return true for primitive types" in {
    for {
      primitive <- Primitives
    } TypeNothing <:< primitive shouldBe true
  }

  it should "return true for Any" in {
    TypeNothing <:< implicitly[Type[Any]] shouldBe true
  }

  it should "return true for AnyVal" in {
    TypeNothing <:< implicitly[Type[AnyVal]] shouldBe true
  }

  it should "return true for AnyRef" in {
    TypeNothing <:< implicitly[Type[AnyRef]] shouldBe true
  }

  it should "return true for Null" in {
    TypeNothing <:< implicitly[Type[Null]] shouldBe true
  }

  it should "return true for a reference trait" in {
    TypeNothing <:< TypeRefTrait shouldBe true
  }

  it should "return true for an universal trait" in {
    TypeNothing <:< TypeUniversalTrait shouldBe true
  }

  it should "return true for a value class" in {
    TypeNothing <:< TypeValClass shouldBe true
  }

  "=:=" should "be reflexive" in {
    TypeNothing =:= TypeNothing shouldBe true
  }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeNothing =:= primitive shouldBe false
  }

  it should "return false for Any" in {
    TypeNothing =:= implicitly[Type[Any]] shouldBe false
  }

  it should "return false for AnyVal" in {
    TypeNothing =:= implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for AnyRef" in {
    TypeNothing =:= implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Null" in {
    TypeNothing =:= implicitly[Type[Null]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeNothing =:= TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeNothing =:= TypeUniversalTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeNothing =:= TypeValClass shouldBe false
  }
}
