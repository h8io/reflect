package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util as ju

class TypeTest extends AnyFlatSpec with Matchers {
  "<:<" should "work for a primitive type" in {
    implicitly[Type[Int]] <:< implicitly[Type[Int]] shouldBe true

    implicitly[Type[Int]] <:< implicitly[Type[Any]] shouldBe true
    implicitly[Type[Int]] <:< implicitly[Type[AnyVal]] shouldBe true
    implicitly[Type[Int]] <:< implicitly[Type[AnyRef]] shouldBe false
    implicitly[Type[Int]] <:< implicitly[Type[Integer]] shouldBe false
    implicitly[Type[Integer]] <:< implicitly[Type[Int]] shouldBe false
  }

  it should "work for a plain reference type" in {
    implicitly[Type[String]] <:< implicitly[Type[String]] shouldBe true

    implicitly[Type[String]] <:< implicitly[Type[AnyVal]] shouldBe false
    implicitly[Type[String]] <:< implicitly[Type[AnyRef]] shouldBe true
    implicitly[Type[String]] <:< implicitly[Type[CharSequence]] shouldBe true
    implicitly[Type[String]] <:< implicitly[Type[Nothing]] shouldBe false
    implicitly[Type[CharSequence]] <:< implicitly[Type[String]] shouldBe false
  }

  it should "work for a reference type with invariant type parameters" in {
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.Collection[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.Collection[String]]] shouldBe true
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.Collection[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] <:< implicitly[Type[ju.Collection[String]]] shouldBe false

    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.List[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.List[String]]] shouldBe true
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.List[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] <:< implicitly[Type[ju.List[String]]] shouldBe false

    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.ArrayList[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.ArrayList[String]]] shouldBe false
    implicitly[Type[ju.List[String]]] <:< implicitly[Type[ju.ArrayList[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] <:< implicitly[Type[ju.ArrayList[String]]] shouldBe false
  }

  it should "work for a reference type with covariant type parameters" in {
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Iterable[CharSequence]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Iterable[String]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Iterable[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] <:< implicitly[Type[Iterable[String]]] shouldBe false

    implicitly[Type[Seq[String]]] <:< implicitly[Type[Seq[CharSequence]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Seq[String]]] shouldBe true
    implicitly[Type[Seq[String]]] <:< implicitly[Type[Seq[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] <:< implicitly[Type[Seq[String]]] shouldBe false

    implicitly[Type[Seq[String]]] <:< implicitly[Type[List[CharSequence]]] shouldBe false
    implicitly[Type[Seq[String]]] <:< implicitly[Type[List[String]]] shouldBe false
    implicitly[Type[Seq[String]]] <:< implicitly[Type[List[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] <:< implicitly[Type[List[String]]] shouldBe false
  }

  it should "work for a reference type with contravariant type parameters" in {
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[CharSequence => Unit]] shouldBe false
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[String => Unit]] shouldBe true
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[Nothing => Unit]] shouldBe true
    implicitly[Type[PartialFunction[CharSequence, Unit]]] <:< implicitly[Type[String => Unit]] shouldBe true

    implicitly[Type[PartialFunction[String, Unit]]] <:<
      implicitly[Type[PartialFunction[CharSequence, Unit]]] shouldBe false
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[PartialFunction[String, Unit]]] shouldBe true
    implicitly[Type[PartialFunction[String, Unit]]] <:< implicitly[Type[PartialFunction[Nothing, Unit]]] shouldBe true
    implicitly[Type[PartialFunction[CharSequence, Unit]]] <:<
      implicitly[Type[PartialFunction[String, Unit]]] shouldBe true

    implicitly[Type[String => Unit]] <:< implicitly[Type[PartialFunction[CharSequence, Unit]]] shouldBe false
    implicitly[Type[String => Unit]] <:< implicitly[Type[PartialFunction[String, Unit]]] shouldBe false
    implicitly[Type[String => Unit]] <:< implicitly[Type[PartialFunction[Nothing, Unit]]] shouldBe false
    implicitly[Type[CharSequence => Unit]] <:< implicitly[Type[PartialFunction[String, Unit]]] shouldBe false
  }

  "=:=" should "work for a primitive type" in {
    implicitly[Type[Int]] =:= implicitly[Type[Int]] shouldBe true

    implicitly[Type[Int]] =:= implicitly[Type[Any]] shouldBe false
    implicitly[Type[Int]] =:= implicitly[Type[AnyVal]] shouldBe false
    implicitly[Type[Int]] =:= implicitly[Type[AnyRef]] shouldBe false
    implicitly[Type[Int]] =:= implicitly[Type[Integer]] shouldBe false
    implicitly[Type[Integer]] =:= implicitly[Type[Int]] shouldBe false
  }

  it should "work for a plain reference type" in {
    implicitly[Type[String]] =:= implicitly[Type[String]] shouldBe true

    implicitly[Type[String]] =:= implicitly[Type[AnyVal]] shouldBe false
    implicitly[Type[String]] =:= implicitly[Type[AnyRef]] shouldBe false
    implicitly[Type[String]] =:= implicitly[Type[CharSequence]] shouldBe false
    implicitly[Type[String]] =:= implicitly[Type[Nothing]] shouldBe false
    implicitly[Type[CharSequence]] =:= implicitly[Type[String]] shouldBe false
  }

  it should "work for a reference type with invariant type parameters" in {
    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.Collection[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.Collection[String]]] shouldBe false
    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.Collection[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] =:= implicitly[Type[ju.Collection[String]]] shouldBe false

    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.List[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.List[String]]] shouldBe true
    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.List[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] =:= implicitly[Type[ju.List[String]]] shouldBe false

    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.ArrayList[CharSequence]]] shouldBe false
    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.ArrayList[String]]] shouldBe false
    implicitly[Type[ju.List[String]]] =:= implicitly[Type[ju.ArrayList[Nothing]]] shouldBe false
    implicitly[Type[ju.List[CharSequence]]] =:= implicitly[Type[ju.ArrayList[String]]] shouldBe false
  }

  it should "work for a reference type with covariant type parameters" in {
    implicitly[Type[Seq[String]]] =:= implicitly[Type[Iterable[CharSequence]]] shouldBe false
    implicitly[Type[Seq[String]]] =:= implicitly[Type[Iterable[String]]] shouldBe false
    implicitly[Type[Seq[String]]] =:= implicitly[Type[Iterable[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] =:= implicitly[Type[Iterable[String]]] shouldBe false

    implicitly[Type[Seq[String]]] =:= implicitly[Type[Seq[CharSequence]]] shouldBe false
    implicitly[Type[Seq[String]]] =:= implicitly[Type[Seq[String]]] shouldBe true
    implicitly[Type[Seq[String]]] =:= implicitly[Type[Seq[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] =:= implicitly[Type[Seq[String]]] shouldBe false

    implicitly[Type[Seq[String]]] =:= implicitly[Type[List[CharSequence]]] shouldBe false
    implicitly[Type[Seq[String]]] =:= implicitly[Type[List[String]]] shouldBe false
    implicitly[Type[Seq[String]]] =:= implicitly[Type[List[Nothing]]] shouldBe false
    implicitly[Type[Seq[CharSequence]]] =:= implicitly[Type[List[String]]] shouldBe false
  }

  it should "work for a reference type with contravariant type parameters" in {
    implicitly[Type[PartialFunction[String, Unit]]] =:= implicitly[Type[CharSequence => Unit]] shouldBe false
    implicitly[Type[PartialFunction[String, Unit]]] =:= implicitly[Type[String => Unit]] shouldBe false
    implicitly[Type[PartialFunction[String, Unit]]] =:= implicitly[Type[Nothing => Unit]] shouldBe false
    implicitly[Type[PartialFunction[CharSequence, Unit]]] =:= implicitly[Type[String => Unit]] shouldBe false

    implicitly[Type[PartialFunction[String, Unit]]] =:=
      implicitly[Type[PartialFunction[CharSequence, Unit]]] shouldBe false
    implicitly[Type[PartialFunction[String, Unit]]] =:= implicitly[Type[PartialFunction[String, Unit]]] shouldBe true
    implicitly[Type[PartialFunction[String, Unit]]] =:= implicitly[Type[PartialFunction[Nothing, Unit]]] shouldBe false
    implicitly[Type[PartialFunction[CharSequence, Unit]]] =:=
      implicitly[Type[PartialFunction[String, Unit]]] shouldBe false

    implicitly[Type[String => Unit]] =:= implicitly[Type[PartialFunction[CharSequence, Unit]]] shouldBe false
    implicitly[Type[String => Unit]] =:= implicitly[Type[PartialFunction[String, Unit]]] shouldBe false
    implicitly[Type[String => Unit]] =:= implicitly[Type[PartialFunction[Nothing, Unit]]] shouldBe false
    implicitly[Type[CharSequence => Unit]] =:= implicitly[Type[PartialFunction[String, Unit]]] shouldBe false
  }

  "hashCode" should "return the same value for equal types" in {
    implicitly[Type[Any]].hashCode shouldBe implicitly[Type[Any]].hashCode
    implicitly[Type[String]].hashCode shouldBe implicitly[Type[String]].hashCode
    implicitly[Type[List[AnyRef]]].hashCode shouldBe implicitly[Type[List[AnyRef]]].hashCode
  }

  "equals" should "should compare underlying objects" in {
    implicitly[Type[Any]] == implicitly[Type[Any]] shouldBe true
    implicitly[Type[Int]] == implicitly[Type[Int]] shouldBe true
    implicitly[Type[String]] == implicitly[Type[String]] shouldBe true
    implicitly[Type[List[AnyRef]]] == implicitly[Type[List[AnyRef]]] shouldBe true
    implicitly[Type[List[AnyRef]]] == implicitly[Type[List[String]]] shouldBe false
    implicitly[Type[List[String]]] == implicitly[Type[List[String]]] shouldBe true
    implicitly[Type[List[String]]] == implicitly[Type[Seq[String]]] shouldBe false
    implicitly[Type[List[AnyRef]]] == implicitly[Type[PartialFunction[String, Unit]]] shouldBe false
    implicitly[Type[AnyRef]] == new Object() shouldBe false
  }

//  "toString" should "return the type name" in {
//    implicitly[Type[Any]].toString shouldBe "Any"
//    implicitly[Type[AnyVal]].toString shouldBe "AnyVal"
//    implicitly[Type[AnyRef]].toString shouldBe "AnyRef"
//    implicitly[Type[Object]].toString shouldBe "AnyRef"
//    implicitly[Type[Int]].toString shouldBe "Int"
//    implicitly[Type[String]].toString shouldBe "java.lang.String"
//    implicitly[Type[Integer]].toString shouldBe "Integer"
//    implicitly[Type[List[Int]]].toString shouldBe "List[+Int]"
//    implicitly[Type[List[Integer]]].toString shouldBe "List[+Integer]"
//    implicitly[Type[Integer => String]].toString shouldBe "Function1[-Integer, +String]"
//    implicitly[Type[(java.lang.Long, java.time.Instant)]].toString shouldBe "Tuple2[+Long, +java.time.Instant]"
//  }
}
