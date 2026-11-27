package h8io.reflect

import h8io.reflect.test.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Each answer is checked against the compiler's, which is the specification. */
class TypeTest extends AnyFlatSpec with Matchers {
  "AnyRef" should "be above reference types only" in {
    Type[Int] <:< Type[AnyRef] shouldBe false
    assertTypeError("implicitly[Int <:< AnyRef]")
    Type[ValClass] <:< Type[AnyRef] shouldBe false
    assertTypeError("implicitly[ValClass <:< AnyRef]")
    Type[UniversalTrait] <:< Type[AnyRef] shouldBe false
    assertTypeError("implicitly[UniversalTrait <:< AnyRef]")
    Type[RefTrait] <:< Type[AnyRef] shouldBe true
    assertCompiles("implicitly[RefTrait <:< AnyRef]")
    Type[String] <:< Type[AnyRef] shouldBe true
    assertCompiles("implicitly[String <:< AnyRef]")
    Type[AnyRef] =:= Type[Object] shouldBe true
    assertCompiles("implicitly[AnyRef =:= Object]")
  }

  "AnyVal" should "be above primitives and value classes only" in {
    Type[Int] <:< Type[AnyVal] shouldBe true
    assertCompiles("implicitly[Int <:< AnyVal]")
    Type[ValClass] <:< Type[AnyVal] shouldBe true
    assertCompiles("implicitly[ValClass <:< AnyVal]")
    Type[UniversalTrait] <:< Type[AnyVal] shouldBe false
    assertTypeError("implicitly[UniversalTrait <:< AnyVal]")
    Type[String] <:< Type[AnyVal] shouldBe false
    assertTypeError("implicitly[String <:< AnyVal]")
  }

  "Any" should "be above everything" in {
    for (t <- TypeUniversalTrait :: TypeValClass :: TypeRefTrait :: Type[Null] :: Type[Nothing] :: Primitives)
      t <:< Type[Any] shouldBe true
    assertCompiles("implicitly[UniversalTrait <:< Any]")
    Type[Any] <:< Type[AnyRef] shouldBe false
    assertTypeError("implicitly[Any <:< AnyRef]")
  }

  "Universal traits" should "be above the classes that extend them" in {
    TypeValClass <:< TypeUniversalParentTrait shouldBe true
    assertCompiles("implicitly[ValClass <:< UniversalParentTrait]")
    TypeRefTrait <:< TypeUniversalParentTrait shouldBe true
    assertCompiles("implicitly[RefTrait <:< UniversalParentTrait]")
    TypeUniversalTrait <:< TypeUniversalParentTrait shouldBe true
    assertCompiles("implicitly[UniversalTrait <:< UniversalParentTrait]")
    TypeUniversalParentTrait <:< TypeUniversalTrait shouldBe false
    assertTypeError("implicitly[UniversalParentTrait <:< UniversalTrait]")
  }

  "Primitives" should "be unrelated to each other and to their boxes" in {
    for {
      a <- Primitives
      b <- Primitives
    } {
      a <:< b shouldBe (a eq b)
      a =:= b shouldBe (a eq b)
    }
    Type[Int] <:< Type[Long] shouldBe false
    assertTypeError("implicitly[Int <:< Long]")
    Type[Int] <:< Type[Integer] shouldBe false
    assertTypeError("implicitly[Int <:< Integer]")
  }

  "Null" should "be below everything that is not an AnyVal" in {
    Type[Null] <:< Type[Int] shouldBe false
    assertTypeError("implicitly[Null <:< Int]")
    Type[Null] <:< TypeValClass shouldBe false
    assertTypeError("implicitly[Null <:< ValClass]")
    Type[Null] <:< Type[AnyVal] shouldBe false
    assertTypeError("implicitly[Null <:< AnyVal]")
    Type[Null] <:< TypeUniversalTrait shouldBe true
    assertCompiles("implicitly[Null <:< UniversalTrait]")
    Type[Null] <:< Type[String] shouldBe true
    assertCompiles("implicitly[Null <:< String]")
    Type[Null] <:< Type[Null] shouldBe true
    assertCompiles("implicitly[Null <:< Null]")
    Type[Null] <:< Type[Nothing] shouldBe false
    assertTypeError("implicitly[Null <:< Nothing]")
    Type[String] <:< Type[Null] shouldBe false
    assertTypeError("implicitly[String <:< Null]")
  }

  "Nothing" should "be below everything, Null included" in {
    for (t <- TypeUniversalTrait :: TypeValClass :: Type[Null] :: Type[Nothing] :: Primitives)
      Type[Nothing] <:< t shouldBe true
    assertCompiles("implicitly[Nothing <:< Null]")
    Type[Int] <:< Type[Nothing] shouldBe false
    assertTypeError("implicitly[Int <:< Nothing]")
  }

  "Type arguments" should "be compared by the same rules, with variance" in {
    Type[List[Int]] <:< Type[List[AnyRef]] shouldBe false
    assertTypeError("implicitly[List[Int] <:< List[AnyRef]]")
    Type[List[ValClass]] <:< Type[List[AnyRef]] shouldBe false
    assertTypeError("implicitly[List[ValClass] <:< List[AnyRef]]")
    Type[Option[UniversalTrait]] <:< Type[Option[AnyRef]] shouldBe false
    assertTypeError("implicitly[Option[UniversalTrait] <:< Option[AnyRef]]")
    Type[List[Null]] <:< Type[List[Int]] shouldBe false
    assertTypeError("implicitly[List[Null] <:< List[Int]]")
    Type[List[Null]] <:< Type[List[String]] shouldBe true
    assertCompiles("implicitly[List[Null] <:< List[String]]")
    Type[Contra[Any]] <:< Type[Contra[Int]] shouldBe true
    assertCompiles("implicitly[Contra[Any] <:< Contra[Int]]")
    Type[Contra[Int]] <:< Type[Contra[Any]] shouldBe false
    assertTypeError("implicitly[Contra[Int] <:< Contra[Any]]")
    Type[In[Int]] <:< Type[In[Any]] shouldBe false
    assertTypeError("implicitly[In[Int] <:< In[Any]]")
    Type[In[AnyRef]] =:= Type[In[Object]] shouldBe true
    assertCompiles("implicitly[In[AnyRef] =:= In[Object]]")
  }

  "Stage" should "compare its input contravariantly" in {
    Type[Stage[AnyRef, Int, Nothing]] <:< Type[Stage[Int, Int, Nothing]] shouldBe false
    assertTypeError("implicitly[Stage[AnyRef, Int, Nothing] <:< Stage[Int, Int, Nothing]]")
    Type[Stage[Any, Int, Nothing]] <:< Type[Stage[String, Int, Nothing]] shouldBe true
    assertCompiles("implicitly[Stage[Any, Int, Nothing] <:< Stage[String, Int, Nothing]]")
  }

  "Base types" should "carry substituted arguments" in {
    Type[List[Int]] <:< Type[Seq[Any]] shouldBe true
    assertCompiles("implicitly[List[Int] <:< Seq[Any]]")
    Type[List[String]] <:< Type[PartialFunction[Int, CharSequence]] shouldBe true
    assertCompiles("implicitly[List[String] <:< PartialFunction[Int, CharSequence]]")
    Type[List[Int]] <:< Type[Set[Int]] shouldBe false
    assertTypeError("implicitly[List[Int] <:< Set[Int]]")
    Type[SelfRef] <:< Type[Comparable[SelfRef]] shouldBe true
    assertCompiles("implicitly[SelfRef <:< Comparable[SelfRef]]")
    Type[CoTwice] <:< Type[Co[Int]] shouldBe true
    assertCompiles("implicitly[CoTwice <:< Co[Int]]")
    Type[NullBase] <:< Type[Co[String]] shouldBe true
    assertCompiles("implicitly[NullBase <:< Co[String]]")
    Type[NullBase] <:< Type[Co[Int]] shouldBe false
    assertTypeError("implicitly[NullBase <:< Co[Int]]")
    Type[Nest[String]] <:< Type[Co[Seq[CharSequence]]] shouldBe true
    assertCompiles("implicitly[Nest[String] <:< Co[Seq[CharSequence]]]")
    Type[Nest[String]] <:< Type[Co[Seq[Int]]] shouldBe false
    assertTypeError("implicitly[Nest[String] <:< Co[Seq[Int]]]")
    Type[None.type] <:< Type[Option[Int]] shouldBe true
    assertCompiles("implicitly[None.type <:< Option[Int]]")
    Type[java.util.ArrayList[String]] <:< Type[java.util.List[String]] shouldBe true
    assertCompiles("implicitly[java.util.ArrayList[String] <:< java.util.List[String]]")
    Type[java.util.ArrayList[String]] <:< Type[java.util.List[CharSequence]] shouldBe false
    assertTypeError("implicitly[java.util.ArrayList[String] <:< java.util.List[CharSequence]]")
  }

  "Aliases" should "be dealiased" in {
    type IntList = List[Int]
    Type[IntList] =:= Type[scala.collection.immutable.List[Int]] shouldBe true
    assertCompiles("implicitly[IntList =:= scala.collection.immutable.List[Int]]")
  }

  "Classes nested in objects" should "be supported" in {
    Type[Holder.Nested] <:< Type[AnyRef] shouldBe true
    assertCompiles("implicitly[Holder.Nested <:< AnyRef]")
  }

  "Annotations" should "be dropped" in {
    Type[Int @TypeAnnotation] =:= Type[Int] shouldBe true
    assertCompiles("implicitly[(Int @TypeAnnotation) =:= Int]")
  }

  "toString" should "show the type" in {
    Type[Map[String, Int]].toString shouldBe "scala.collection.immutable.Map[java.lang.String, scala.Int]"
    Type[Nothing].toString shouldBe "Nothing"
    Type[Null].toString shouldBe "Null"
  }

  "Types outside the subset" should "be compile errors" in {
    assertTypeError("Type[RefTrait { def x: Int }]")
    assertTypeError("Type[1]")
    assertTypeError("def f[A]: Type[List[A]] = Type[List[A]]")
    assertTypeError("class Local; Type[Local]")
    assertTypeError("class Outer { class Inner }; Type[Outer#Inner]")
    assertTypeError("Type[Fix[List]]")
    assertTypeError("Type[Fix[Option]]")
    assertTypeError("Type[java.util.List[?]]")
  }
}
