package h8io.reflect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeValTest extends AnyFlatSpec with Matchers {
  import types.*

  "TypeValClass" should "be a TypeVal" in { TypeValClass shouldBe a[TypeVal[?]] }
}
