package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeValTest extends AnyFlatSpec with Matchers {
  import test.*

  "TypeValClass" should "be a TypeVal" in { TypeValClass shouldBe a[TypeVal[?]] }

  "toString" should "produce a correct string representation" in {
    TypeValClass.toString shouldBe classOf[ValClass].getName
  }

  "<:<" should "be reflexive" in {
    TypeValClass <:< TypeValClass shouldBe true
  }

  it should "return false for primitive types" in {
    for {
      primitive <- Primitives
    } TypeValClass <:< primitive shouldBe false
  }

  it should "return true for Any" in {
    TypeValClass <:< implicitly[Type[Any]] shouldBe true
  }

  it should "return true for AnyVal" in {
    TypeValClass <:< implicitly[Type[AnyVal]] shouldBe true
  }

  it should "return false for AnyRef" in {
    TypeValClass <:< implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeValClass <:< implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeValClass <:< implicitly[Type[Null]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeValClass <:< TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeValClass <:< TypeUniversalTrait shouldBe false
  }

  it should "return true for an universal parent trait" in {
    TypeValClass <:< TypeUniversalParentTrait shouldBe true
  }

  "=:=" should "be reflexive" in {
    TypeValClass =:= TypeValClass shouldBe true
  }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeValClass =:= primitive shouldBe false
  }

  it should "return false for Any" in {
    TypeValClass =:= implicitly[Type[Any]] shouldBe false
  }

  it should "return false for AnyVal" in {
    TypeValClass =:= implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for AnyRef" in {
    TypeValClass =:= implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeValClass =:= implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeValClass =:= implicitly[Type[Null]] shouldBe false
  }

  it should "return false for a reference trait" in {
    TypeValClass =:= TypeRefTrait shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeValClass =:= TypeUniversalTrait shouldBe false
  }

  it should "return false for an universal parent trait" in {
    TypeValClass =:= TypeUniversalParentTrait shouldBe false
  }
}
