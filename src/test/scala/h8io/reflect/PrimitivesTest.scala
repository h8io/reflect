package h8io.reflect

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.runtime.BoxedUnit

sealed abstract class PrimitiveSpec[T <: AnyVal, B <: AnyRef](repr: String, expected: Type[T])(
    implicit tp: Type[T], boxed: Type[B])
    extends AnyFlatSpec with Matchers with Inside {
  import types.*

  "implicit value" should "has a correct type" in {
    tp shouldBe a[Primitive[?, ?]]
    tp should be theSameInstanceAs expected
  }

  "toString" should "produce a correct string representation" in { tp.toString shouldBe repr }

  "boxed" should "return a correct Type" in
    inside(tp) { case p: Primitive[?, ?] => p.boxed shouldBe boxed }

  "<:<" should "return true for the self type" in { tp <:< tp shouldBe true }

  it should "return false for other primitive types" in {
    for {
      other <- Primitives
      if other != tp
    } tp <:< other shouldBe false
  }

  it should "return true for Any" in { tp <:< implicitly[Type[Any]] shouldBe true }

  it should "return true for AnyVal" in { tp <:< implicitly[Type[AnyVal]] shouldBe true }

  it should "return false for AnyRef" in { tp <:< implicitly[Type[AnyRef]] shouldBe false }

  it should "return false for Nothing" in { tp <:< implicitly[Type[Nothing]] shouldBe false }

  it should "return false for Null" in { tp <:< implicitly[Type[Null]] shouldBe false }

  it should "return false for a reference trait" in { tp =:= TypeRefTrait shouldBe false }

  it should "return false for an universal trait" in { tp =:= TypeUniversalTrait shouldBe false }

  it should "return false for a value class" in { tp =:= TypeValClass shouldBe false }

  "=:=" should "return true for the self type" in { tp =:= tp shouldBe true }

  it should "return false for other primitive types" in {
    for {
      other <- Primitives
      if other != tp
    } tp =:= other shouldBe false
  }

  it should "return false for Any" in { tp =:= implicitly[Type[Any]] shouldBe false }

  it should "return false for AnyVal" in { tp =:= implicitly[Type[AnyVal]] shouldBe false }

  it should "return false for AnyRef" in { tp =:= implicitly[Type[AnyRef]] shouldBe false }

  it should "return false for Nothing" in { tp =:= implicitly[Type[Nothing]] shouldBe false }

  it should "return false for Null" in { tp =:= implicitly[Type[Null]] shouldBe false }

  it should "return false for a reference trait" in { tp =:= TypeRefTrait shouldBe false }

  it should "return false for an universal trait" in { tp =:= TypeUniversalTrait shouldBe false }

  it should "return false for a value class" in { tp =:= TypeValClass shouldBe false }
}

class TypeBooleanTest extends PrimitiveSpec[Boolean, java.lang.Boolean]("Boolean", TypeBoolean)

class TypeByteTest extends PrimitiveSpec[Byte, java.lang.Byte]("Byte", TypeByte)

class TypeShortTest extends PrimitiveSpec[Short, java.lang.Short]("Short", TypeShort)

class TypeIntTest extends PrimitiveSpec[Int, java.lang.Integer]("Int", TypeInt)

class TypeLongTest extends PrimitiveSpec[Long, java.lang.Long]("Long", TypeLong)

class TypeFloatTest extends PrimitiveSpec[Float, java.lang.Float]("Float", TypeFloat)

class TypeDoubleTest extends PrimitiveSpec[Double, java.lang.Double]("Double", TypeDouble)

class TypeCharTest extends PrimitiveSpec[Char, java.lang.Character]("Char", TypeChar)

class TypeUnitTest extends PrimitiveSpec[Unit, BoxedUnit]("Unit", TypeUnit)
