package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util as ju

class CovariantTest extends AnyFlatSpec with Matchers {
  "accepts" should "work for a primitive type" in {
    (+implicitly[Type[Int]]).accepts(implicitly[Type[Int]]) shouldBe true

    (+implicitly[Type[Any]]).accepts(implicitly[Type[Int]]) shouldBe true
    (+implicitly[Type[AnyVal]]).accepts(implicitly[Type[Int]]) shouldBe true
    (+implicitly[Type[AnyRef]]).accepts(implicitly[Type[Int]]) shouldBe false
    (+implicitly[Type[Integer]]).accepts(implicitly[Type[Int]]) shouldBe false
    (+implicitly[Type[Int]]).accepts(implicitly[Type[Integer]]) shouldBe false
  }

  it should "work for a plain reference type" in {
    (+implicitly[Type[String]]).accepts(implicitly[Type[String]]) shouldBe true

    (+implicitly[Type[AnyVal]]).accepts(implicitly[Type[String]]) shouldBe false
    (+implicitly[Type[AnyRef]]).accepts(implicitly[Type[String]]) shouldBe true
    (+implicitly[Type[CharSequence]]).accepts(implicitly[Type[String]]) shouldBe true
    (+implicitly[Type[Nothing]]).accepts(implicitly[Type[String]]) shouldBe false
    (+implicitly[Type[String]]).accepts(implicitly[Type[CharSequence]]) shouldBe false
  }

  it should "work for a reference type with invariant type parameters" in {
    (+implicitly[Type[ju.Collection[CharSequence]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false
    (+implicitly[Type[ju.Collection[String]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe true
    (+implicitly[Type[ju.Collection[Nothing]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false
    (+implicitly[Type[ju.Collection[String]]]).accepts(implicitly[Type[ju.List[CharSequence]]]) shouldBe false

    (+implicitly[Type[ju.List[CharSequence]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false
    (+implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe true
    (+implicitly[Type[ju.List[Nothing]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false
    (+implicitly[Type[ju.List[String]]]).accepts(implicitly[Type[ju.List[CharSequence]]]) shouldBe false

    (+implicitly[Type[ju.ArrayList[CharSequence]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false
    (+implicitly[Type[ju.ArrayList[String]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false
    (+implicitly[Type[ju.ArrayList[Nothing]]]).accepts(implicitly[Type[ju.List[String]]]) shouldBe false
    (+implicitly[Type[ju.ArrayList[String]]]).accepts(implicitly[Type[ju.List[CharSequence]]]) shouldBe false
  }

  it should "work for a reference type with covariant type parameters" in {
    (+implicitly[Type[Iterable[CharSequence]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe true
    (+implicitly[Type[Iterable[String]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe true
    (+implicitly[Type[Iterable[Nothing]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe false
    (+implicitly[Type[Iterable[String]]]).accepts(implicitly[Type[Seq[CharSequence]]]) shouldBe false

    (+implicitly[Type[Seq[CharSequence]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe true
    (+implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe true
    (+implicitly[Type[Seq[Nothing]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe false
    (+implicitly[Type[Seq[String]]]).accepts(implicitly[Type[Seq[CharSequence]]]) shouldBe false

    (+implicitly[Type[List[CharSequence]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe false
    (+implicitly[Type[List[String]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe false
    (+implicitly[Type[List[Nothing]]]).accepts(implicitly[Type[Seq[String]]]) shouldBe false
    (+implicitly[Type[List[String]]]).accepts(implicitly[Type[Seq[CharSequence]]]) shouldBe false
  }

  it should "work for a reference type with contravariant type parameters" in {
    (+implicitly[Type[CharSequence => Unit]]).accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe false
    (+implicitly[Type[String => Unit]]).accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe true
    (+implicitly[Type[Nothing => Unit]]).accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe true
    (+implicitly[Type[String => Unit]]).accepts(implicitly[Type[PartialFunction[CharSequence, Unit]]]) shouldBe true

    (+implicitly[Type[PartialFunction[CharSequence, Unit]]])
      .accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe false
    (+implicitly[Type[PartialFunction[String, Unit]]])
      .accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe true
    (+implicitly[Type[PartialFunction[Nothing, Unit]]])
      .accepts(implicitly[Type[PartialFunction[String, Unit]]]) shouldBe true
    (+implicitly[Type[PartialFunction[String, Unit]]])
      .accepts(implicitly[Type[PartialFunction[CharSequence, Unit]]]) shouldBe true

    (+implicitly[Type[PartialFunction[CharSequence, Unit]]]).accepts(implicitly[Type[String => Unit]]) shouldBe false
    (+implicitly[Type[PartialFunction[String, Unit]]]).accepts(implicitly[Type[String => Unit]]) shouldBe false
    (+implicitly[Type[PartialFunction[Nothing, Unit]]]).accepts(implicitly[Type[String => Unit]]) shouldBe false
    (+implicitly[Type[PartialFunction[String, Unit]]]).accepts(implicitly[Type[CharSequence => Unit]]) shouldBe false
  }

  "toString" should "return the correct string" in {
    (+implicitly[Type[Instant => Long => String]]).toString shouldBe
      "+Function1[-java.time.Instant, +Function1[-Long, +String]]"
    (+implicitly[Type[(Instant => Long) => String]]).toString shouldBe
      "+Function1[-Function1[+java.time.Instant, -Long], +String]"
  }
}
