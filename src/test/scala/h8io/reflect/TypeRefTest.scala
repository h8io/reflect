package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeRefTest extends AnyFlatSpec with Matchers {
  import types.*

  "TypeRefTrait" should "be a TypeRef" in { TypeRefTrait shouldBe a[TypeRef[?]] }
}
