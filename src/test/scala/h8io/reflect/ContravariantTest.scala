package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util as ju

class ContravariantTest extends AnyFlatSpec with Matchers {
  "accepts" should "work for a primitive type" in {
    (-implicitly[Type[Int]]).accepts(implicitly[Type[Int]]) shouldBe true

    (-implicitly[Type[Int]]).accepts(implicitly[Type[Any]]) shouldBe true
    (-implicitly[Type[Int]]).accepts(implicitly[Type[AnyVal]]) shouldBe true
    (-implicitly[Type[Int]]).accepts(implicitly[Type[AnyRef]]) shouldBe false
    (-implicitly[Type[Int]]).accepts(implicitly[Type[Integer]]) shouldBe false
    (-implicitly[Type[Integer]]).accepts(implicitly[Type[Int]]) shouldBe false
  }

  it should "work for a plain reference type" in {
    (-implicitly[Type[String]]).accepts(implicitly[Type[String]]) shouldBe true

    (-implicitly[Type[String]]).accepts(implicitly[Type[AnyVal]]) shouldBe false
    (-implicitly[Type[String]]).accepts(implicitly[Type[AnyRef]]) shouldBe true
    (-implicitly[Type[String]]).accepts(implicitly[Type[CharSequence]]) shouldBe true
    (-implicitly[Type[String]]).accepts(implicitly[Type[Nothing]]) shouldBe false
    (-implicitly[Type[CharSequence]]).accepts(implicitly[Type[String]]) shouldBe false
  }

  it should "work for a reference type with invariant type parameters" in {
    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.Collection[CharSequence]]]) shouldBe false
    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.Collection[String]]]) shouldBe true
    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.Collection[Nothing]]]) shouldBe false
    (-implicitly[Type[ju.List[CharSequence]]]).accepts(implicitly[Type[ju.Collection[String]]]) shouldBe false

    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.List[CharSequence]]]) shouldBe false
    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe true
    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.List[Nothing]]]) shouldBe false
    (-implicitly[Type[ju.List[CharSequence]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false

    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.ArrayList[CharSequence]]]) shouldBe false
    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.ArrayList[String]]]) shouldBe false
    (-implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.ArrayList[Nothing]]]) shouldBe false
    (-implicitly[Type[ju.List[CharSequence]]]).accepts(implicitly[Type[ju.ArrayList[String]]]) shouldBe false
  }

  it should "work for a reference type with covariant type parameters" in {
    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Iterable[CharSequence]]]) shouldBe true
    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Iterable[String]]]) shouldBe true
    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Iterable[Nothing]]]) shouldBe false
    (-implicitly[Type[Seq[CharSequence]]]).accepts(implicitly[Type[Iterable[String]]]) shouldBe false

    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Seq[CharSequence]]]) shouldBe true
    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe true
    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Seq[Nothing]]]) shouldBe false
    (-implicitly[Type[Seq[CharSequence]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe false

    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[List[CharSequence]]]) shouldBe false
    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[List[String]]]) shouldBe false
    (-implicitly[Type[Seq[String]]]).accepts(implicitly[Type[List[Nothing]]]) shouldBe false
    (-implicitly[Type[Seq[CharSequence]]]).accepts(implicitly[Type[List[String]]]) shouldBe false
  }

  it should "work for a reference type with contravariant type parameters" in {
    (-implicitly[Type[PartialFunction[String, Unit]]]).accepts(implicitly[Type[CharSequence => Unit]]) shouldBe false
    (-implicitly[Type[PartialFunction[String, Unit]]]).accepts(implicitly[Type[String => Unit]]) shouldBe true
    (-implicitly[Type[PartialFunction[String, Unit]]]).accepts(implicitly[Type[Nothing => Unit]]) shouldBe true
    (-implicitly[Type[PartialFunction[CharSequence, Unit]]]).accepts(implicitly[Type[String => Unit]]) shouldBe true

    (-implicitly[Type[PartialFunction[String, Unit]]])
      .accepts(implicitly[Type[PartialFunction[CharSequence, Unit]]]) shouldBe false
    (-implicitly[Type[PartialFunction[String, Unit]]])
      .accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe true
    (-implicitly[Type[PartialFunction[String, Unit]]])
      .accepts(implicitly[Type[PartialFunction[Nothing, Unit]]]) shouldBe true
    (-implicitly[Type[PartialFunction[CharSequence, Unit]]])
      .accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe true

    (-implicitly[Type[String => Unit]]).accepts(implicitly[Type[PartialFunction[CharSequence, Unit]]]) shouldBe false
    (-implicitly[Type[String => Unit]]).accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe false
    (-implicitly[Type[String => Unit]]).accepts(implicitly[Type[PartialFunction[Nothing, Unit]]]) shouldBe false
    (-implicitly[Type[CharSequence => Unit]]).accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe false
  }

  "Unary +" should "return object itself" in {
    val variant = +implicitly[Type[String]]
    (+variant) should be theSameInstanceAs variant
  }

  "Unary -" should "a Covariant instance" in {
    val tp = implicitly[Type[String]]
    (-Contravariant(tp)) shouldBe Covariant(tp)
  }

  "toString" should "return the correct string" in {
    (+implicitly[Type[String]]).toString shouldBe "+String"
  }
}
