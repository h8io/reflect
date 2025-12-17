package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeAnyLikeTest extends AnyFlatSpec with Matchers {
  import test.*

  "TypeUniversalTrait" should "be a TypeAnyLike" in { TypeUniversalTrait shouldBe a[TypeAnyLike[?]] }

  "toString" should "produce a correct string representation" in {
    TypeUniversalTrait.toString shouldBe classOf[UniversalTrait].getName
  }

  "<:<" should "be reflexive" in {
    TypeUniversalTrait <:< TypeUniversalTrait shouldBe true
  }

  it should "return false for primitive types" in {
    for {
      primitive <- Primitives
    } TypeUniversalTrait <:< primitive shouldBe false
  }

  it should "return true for Any" in {
    TypeUniversalTrait <:< implicitly[Type[Any]] shouldBe true
  }

  it should "return false for AnyVal" in {
    TypeUniversalTrait <:< implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for AnyRef" in {
    TypeUniversalTrait <:< implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeUniversalTrait <:< implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeUniversalTrait <:< implicitly[Type[Null]] shouldBe false
  }

  it should "return false for an reference trait" in {
    TypeUniversalTrait <:< TypeRefTrait shouldBe false
  }

  it should "return true for an universal parent trait" in {
    TypeUniversalTrait <:< TypeUniversalParentTrait shouldBe true
  }

  it should "return false for a value class" in {
    TypeUniversalTrait <:< TypeValClass shouldBe false
  }

  "=:=" should "be reflexive" in {
    TypeUniversalTrait =:= TypeUniversalTrait shouldBe true
  }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeUniversalTrait =:= primitive shouldBe false
  }

  it should "return false for Any" in {
    TypeUniversalTrait =:= implicitly[Type[Any]] shouldBe false
  }

  it should "return false for AnyVal" in {
    TypeUniversalTrait =:= implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for AnyRef" in {
    TypeUniversalTrait =:= implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeUniversalTrait =:= implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeUniversalTrait =:= implicitly[Type[Null]] shouldBe false
  }

  it should "return false for an reference trait" in {
    TypeUniversalTrait =:= TypeRefTrait shouldBe false
  }

  it should "return false for an universal parent trait" in {
    TypeUniversalTrait =:= TypeUniversalParentTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeUniversalTrait =:= TypeValClass shouldBe false
  }
}
