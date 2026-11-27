package h8io.reflect

import h8io.reflect.internal.Spec
import h8io.reflect.test.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KindTest extends AnyFlatSpec with Matchers {
  private val OptionK = Kind1[Option]

  "apply" should "build the type the compiler names" in {
    OptionK(Type[Int]) =:= Type[Option[Int]] shouldBe true
    Kind2[Tuple2].apply(Type[Int], Type[String]) =:= Type[(Int, String)] shouldBe true
    Kind3[Stage].apply(Type[Int], Type[String], Type[Unit]) =:= Type[Stage[Int, String, Unit]] shouldBe true
    Kind1[List].apply(Type[Int]) =:= Type[List[Int]] shouldBe true
  }

  it should "keep the constructor's variances" in {
    OptionK(Type[Int]) <:< OptionK(Type[Any]) shouldBe true
    Kind1[Contra].apply(Type[Any]) <:< Kind1[Contra].apply(Type[Int]) shouldBe true
  }

  it should "work in generic code" in {
    def f[F[_]: Kind1, A: Type]: Type[F[A]] = Kind1[F].apply(Type[A])
    f[Option, String] =:= Type[Option[String]] shouldBe true
  }

  "unapply" should "go through the base type" in {
    OptionK.unapply(Type[Some[Int]]).get =:= Type[Int] shouldBe true
    OptionK.unapply(Type[None.type]).get =:= Type[Nothing] shouldBe true
    OptionK.unapply(Type[List[Int]]) shouldBe None
    val Some((k, v)) = Kind2[Tuple2].unapply(Type[(Int, String)]): @unchecked
    k =:= Type[Int] shouldBe true
    v =:= Type[String] shouldBe true
  }

  it should "give the least arguments on the bottom types" in {
    OptionK.unapply(Type[Nothing]).get =:= Type[Nothing] shouldBe true
    OptionK.unapply(Type[Null]).get =:= Type[Nothing] shouldBe true
    for (bottom <- List(Type[Nothing], Type[Null])) {
      val Some((i, o, e)) = Kind3[Stage].unapply(bottom): @unchecked
      i =:= Type[Any] shouldBe true
      o =:= Type[Nothing] shouldBe true
      e =:= Type[Nothing] shouldBe true
      i <:< Type[AnyRef] shouldBe false
    }
  }

  it should "find nothing in Null when the constructor is an AnyVal" in {
    Kind1[ValBox].unapply(Type[Null]) shouldBe None
    Kind1[ValBox].unapply(Type[Nothing]).get =:= Type[Nothing] shouldBe true
  }

  "toString" should "show the constructor" in {
    OptionK.toString shouldBe "scala.Option"
  }

  "Constructors outside the subset" should "be compile errors" in {
    assertTypeError("Kind1[Bounded]")
    assertTypeError("Kind1[Fix]")
    assertTypeError("def f[F[_]]: Kind1[F] = Kind1[F]")
    assertTypeError("type E[A] = Either[String, A]; Kind1[E]")
    assertTypeError("type D[A] = Map[A, A]; Kind1[D]")
  }

  "Spec" should "refuse a type parameter outside a base type" in {
    an[IllegalArgumentException] should be thrownBy Spec.tpe[Any](Nil, Spec.T.Param(0))
  }
}
