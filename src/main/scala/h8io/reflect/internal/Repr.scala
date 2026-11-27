package h8io.reflect.internal

/** The variance of a type parameter. */
sealed abstract class Variance

object Variance {
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance
}

/** A type as `Type` holds it: a bottom type or a class applied to arguments. */
private[reflect] sealed abstract class Repr {
  final def <:<(that: Repr): Boolean =
    (this, that) match {
      case (Repr.Nothing, _) => true
      case (_, Repr.Nothing) => false
      case (Repr.Null, Repr.Null) => true
      case (Repr.Null, Repr.Applied(ctor, _)) => ctor.nullable
      case (_: Repr.Applied, Repr.Null) => false
      case (Repr.Applied(left, leftArgs), Repr.Applied(right, rightArgs)) =>
        left.lift(leftArgs, right.name).exists(Repr.conform(right.variances, _, rightArgs))
    }

  final def =:=(that: Repr): Boolean = this <:< that && that <:< this
}

private[reflect] object Repr {
  case object Nothing extends Repr {
    override def toString: String = "Nothing"
  }

  case object Null extends Repr {
    override def toString: String = "Null"
  }

  final case class Applied(ctor: Ctor, args: List[Repr]) extends Repr {
    override def toString: String = if (args.isEmpty) ctor.name else args.mkString(s"${ctor.name}[", ", ", "]")
  }

  private def conform(variances: List[Variance], left: List[Repr], right: List[Repr]): Boolean =
    variances.lazyZip(left).lazyZip(right).forall {
      case (Variance.Covariant, l, r) => l <:< r
      case (Variance.Contravariant, l, r) => r <:< l
      case (Variance.Invariant, l, r) => l =:= r
    }
}

/** A type inside a base type of a class: like [[Repr]], but it may refer to the class's own parameters. */
private[reflect] sealed abstract class Tpl {
  def substitute(args: List[Repr]): Repr
}

private[reflect] object Tpl {
  case object Nothing extends Tpl {
    def substitute(args: List[Repr]): Repr = Repr.Nothing
  }

  case object Null extends Tpl {
    def substitute(args: List[Repr]): Repr = Repr.Null
  }

  final case class Param(index: Int) extends Tpl {
    def substitute(args: List[Repr]): Repr = args(index)
  }

  final case class Applied(ctor: Ctor, args: List[Tpl]) extends Tpl {
    def substitute(args: List[Repr]): Repr = Repr.Applied(ctor, this.args.map(_.substitute(args)))
  }
}

/** A class, a trait or a primitive, as a type constructor of zero or more parameters.
  *
  * @param name
  *   the fully qualified name; two constructors are the same class exactly when their names are equal
  * @param nullable
  *   whether `Null` conforms to the class applied to its own parameters, as the compiler answered it
  * @param bases
  *   every base type except the class itself, keyed by the name of its class, written in terms of this class's
  *   parameters; base types whose class has higher-kinded parameters are left out, since no `Type` can be built for
  *   them
  */
private[reflect] final class Ctor(
    val name: String,
    val variances: List[Variance],
    val nullable: Boolean,
    bases: => Map[String, List[Tpl]]) {
  private lazy val baseTypes = bases

  /** The arguments of the base type of `this[args]` whose class is `target`, if it has one. */
  def lift(args: List[Repr], target: String): Option[List[Repr]] =
    if (target == name) Some(args) else baseTypes.get(target).map(_.map(_.substitute(args)))
}

private[reflect] object Ctor {
  val Any: Ctor = new Ctor("scala.Any", Nil, true, Map.empty)
}
