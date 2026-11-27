package h8io.reflect.test

trait Co[+A]

trait Contra[-A]

trait In[A]

/** A covariant ancestor reached twice, with arguments that conform to each other. */
class CoTwice extends Co[Any] with CoInt

trait CoInt extends Co[Int]

/** A class that occurs in its own base types. */
final class SelfRef extends Comparable[SelfRef] {
  def compareTo(that: SelfRef): Int = 0
}

/** A value class that takes a parameter, so `Null` does not conform to it whatever the argument. */
final class ValBox[A](val value: A) extends AnyVal

trait Bounded[A <: AnyRef]

trait Fix[F[_]]

trait CBox[-A]

/** `Null` and a nested application of the class's own parameter in base types. */
class NullBase extends Co[Null]

class Nest[A] extends Co[List[A]]

object Holder {
  final class Nested
}
