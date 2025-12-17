package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeAnyLikeTest extends AnyFlatSpec with Matchers {
  import test.*

  "TypeUniversalTrait" should "be a TypeAnyLike" in { TypeUniversalTrait shouldBe a[TypeAnyLike[?]] }
}
