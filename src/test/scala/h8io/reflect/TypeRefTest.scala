package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeRefTest extends AnyFlatSpec with Matchers {
  import test.*

  "TypeRefTrait" should "be a TypeRef" in { TypeRefTrait shouldBe a[TypeRef[?]] }

  "Type of String" should "be a TypeRef" in { implicitly[Type[String]] shouldBe a[TypeRef[?]] }

  "toString" should "produce a correct string representation" in {
    TypeRefTrait.toString shouldBe classOf[RefTrait].getName
  }

  "<:<" should "be reflexive" in {
    TypeRefTrait <:< TypeRefTrait shouldBe true
  }

  it should "return false for primitive types" in {
    for {
      primitive <- Primitives
    } TypeRefTrait <:< primitive shouldBe false
  }

  it should "return true for Any" in {
    TypeRefTrait <:< implicitly[Type[Any]] shouldBe true
  }

  it should "return false for AnyVal" in {
    TypeRefTrait <:< implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return true for AnyRef" in {
    TypeRefTrait <:< implicitly[Type[AnyRef]] shouldBe true
  }

  it should "return false for Nothing" in {
    TypeRefTrait <:< implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeRefTrait <:< implicitly[Type[Null]] shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeRefTrait <:< TypeUniversalTrait shouldBe false
  }

  it should "return true for an universal parent trait" in {
    TypeRefTrait <:< TypeUniversalParentTrait shouldBe true
  }

  it should "return false for a value class" in {
    TypeRefTrait <:< TypeValClass shouldBe false
  }

  "=:=" should "be reflexive" in {
    TypeRefTrait =:= TypeRefTrait shouldBe true
  }

  it should "return false for other primitive types" in {
    for {
      primitive <- Primitives
    } TypeRefTrait =:= primitive shouldBe false
  }

  it should "return false for Any" in {
    TypeRefTrait =:= implicitly[Type[Any]] shouldBe false
  }

  it should "return false for AnyVal" in {
    TypeRefTrait =:= implicitly[Type[AnyVal]] shouldBe false
  }

  it should "return false for AnyRef" in {
    TypeRefTrait =:= implicitly[Type[AnyRef]] shouldBe false
  }

  it should "return false for Nothing" in {
    TypeRefTrait =:= implicitly[Type[Nothing]] shouldBe false
  }

  it should "return false for Null" in {
    TypeRefTrait =:= implicitly[Type[Null]] shouldBe false
  }

  it should "return false for an universal trait" in {
    TypeRefTrait =:= TypeUniversalTrait shouldBe false
  }

  it should "return false for an universal parent trait" in {
    TypeRefTrait =:= TypeUniversalParentTrait shouldBe false
  }

  it should "return false for a value class" in {
    TypeRefTrait =:= TypeValClass shouldBe false
  }
}
