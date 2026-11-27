package h8io.reflect.test

trait CBoxInt extends CBox[Int]

trait CBoxString extends CBox[String]

/** Its base type is `CBox[Int | String]` on Scala 3; Scala 2 rejects the class. */
class CBoxBoth extends CBoxInt with CBoxString
