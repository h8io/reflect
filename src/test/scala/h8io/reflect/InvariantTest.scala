package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InvariantTest extends AnyFlatSpec with Matchers {
  "apply" should "return the same Type instance" in {
    val tp = implicitly[Type[String]]
    Invariant(tp) shouldBe tp
  }

  "unapply" should "return the same Type instance" in {
    val tp = implicitly[Type[Int]]
    tp should matchPattern { case Invariant(`tp`) => }
  }
}
