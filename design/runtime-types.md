# Runtime types: what reflect should become

Status: **discussion.** The API shape is agreed (see "The API"); the implementation direction is
not. Written 2026-11-25.

## The goal

What is actually wanted from `reflect`:

1. **Build types at runtime** from types and type constructors (higher-kinded types).
2. **Compare types** (`<:<`, `=:=`), correctly at every depth, including inside type arguments.
3. **Get types implicitly** (`implicit` on 2.13, the same or `given` on 3).

The current `Type` only does (2), and only at the top level. The consumer is Xi: its
`design/design.md` §7 and §9 need to take a type apart (`typeArgs`, `withoutArgs`), build a new
one (`combine`) and compare it. Examples there: `Unlift` maps `Option[T]` to `T`, `Swap` maps
`(L, R)` to `(R, L)`, `And`/`Or` build `(LO, RO)`/`Either[LO, RO]`, and stage signatures like
`Stage[-I, O, +E]` are compared at the seams. Today `Stage[AnyRef, Int] <: Stage[Int, Int]` comes out
`true` through `Type`.

## What was measured

All probes ran on izumi-reflect 3.0.11, Scala 2.13.18 and 3.9.0. Both versions gave identical
answers everywhere. The probe sources are in the appendix.

### izumi can do (1) and (3)

Building types at runtime is izumi's main feature (it exists for distage and ZIO).

| What | How | Result |
|---|---|---|
| implicit in generic code | `def gen[F[_]: TagK, A: Tag] = Tag[F[A]]` | `gen[List, Int] =:= Tag[List[Int]]` |
| arity 2 | `def gen2[F[_, _]: TagKK, A: Tag, B: Tag]` | `Either[String, Int]` |
| runtime, from values | `TagK[List].tag.combine(Tag[String].tag)` | `=:= List[String]`, `<:< List[Any]`, `<:< Seq[CharSequence]` |
| nested | `TagK[Option].tag.combine(TagK[List].tag.combine(Tag[Int].tag))` | `=:= Option[List[Int]]` |
| partial application | `TagKK[Either].tag.combine(Tag[String].tag)` | `λ %1 → Either[+String,+1]` |
| the known bug | `Tag[List[Int]].tag <:< Tag[List[AnyRef]].tag` | `true` (wrong) |

On the JVM, Scala 2 and 3 have no other library for this: scala-reflect's `TypeTag` is Scala 2
only, and Scala 3 has no runtime reflection at all.

### The current `Type` design does not survive runtime construction

`Type` decides the AnyVal/AnyRef boundary from the upper bound of `T` when the implicit is
picked. That breaks once types are built at runtime:

- For an abstract `F[A]` the bound is `Any`, so it lands in `TypeAnyLike`. A `List[Int]` built
  that way is not a `TypeRef`.
- `Int` reached through `Id[Int]`, or taken out of a type argument, is a wrapper, not `TypeInt`.
  The singletons compare with `eq`, so `=:=` says `false`.
- `LWeakTag` cannot be built generically from `TagK`; that needs `Tag`.

### Is the tag enough to tell AnyVal from AnyRef?

**Only partly.** The public `tag <:< Tag[AnyVal].tag` is right for every type probed,
runtime-combined ones included, except `Null`:

| Type | `<:< AnyVal` | Correct |
|---|---|---|
| `Int`, `Unit`, value class | true | yes |
| `Id[ValClass]` via `Tag.appliedTag` / `combine` | true | yes |
| `String`, `Integer`, `List[Int]`, ref class, ref trait, `AnyRef` | false | yes |
| universal trait (and its universal parent) | false | yes |
| `Nothing` | true | yes |
| `Null` | true | **no** |

The tag's inheritance database (`idb`) does list `AnyVal` as a parent of primitives and value
classes, but `idb` and `basesdb` are `private[reflect]`. They can only be read from inside package
`izumi.reflect`, so they are not usable; the public `<:<` gives the same information anyway.

**What the tag cannot do: tell a universal trait from an AnyRef trait.** Both are "not AnyVal",
both are JVM interfaces. A universal trait's `idb` holds only its universal parents, an AnyRef
trait's is empty, and `Any` is recorded nowhere. Consequences:

- `Null` anywhere (`Null <: Int`, `List[Null] <: List[Int]`): fixable at runtime with Scala's rule
  "`Null <: X` iff `X` is not a subtype of `AnyVal`". But that rule makes `Null <: UniversalTrait`
  `true`, which is the opposite of what `TypeNull` says today (see the open decisions).
- `Int` or a value class against `AnyRef`, in type arguments too: fixable with the AnyVal test.
- `UniversalTrait <: AnyRef` and `Option[UniversalTrait] <: Option[AnyRef]`: **not fixable from the tag.**
  At the top level the statically picked implicit still knows. Inside arguments built by
  `combine`, nothing does.

### Nothing

izumi itself gets `Nothing` right:

- `Nothing <:` `Int`, value class, `String`, `Null` and universal trait are all `true`.
- `Id[Nothing] =:= Nothing` and `Option ∘ Nothing =:= Option[Nothing]`.
- `List[Nothing] <:` `List[Int]` and `List[ValClass]` are `true`.

The only error is `Null <: Nothing` = `true`, which is the Null bug again. The trap is in our own
classification. Because izumi says `Nothing <: AnyVal`, a naive classifier puts `Nothing` into the
AnyVal branch, and the cross-branch shortcut then answers `Nothing <: String` = `false`. That is the
reason `TypeNothing` exists already.

Proposed answer, if izumi stays: **canonicalise at construction.** Any `Type` made from a tag,
`combine` included, is first checked with `=:=` against `Nothing`, `Null`, `Any`, `AnyVal`, `AnyRef`
and the nine primitives, and becomes the singleton on a match. Only then is it classified through
`<:< AnyVal`. That keeps the singletons' table and their `eq` sound, and costs up to 14 `=:=` per
construction (a name check first can short-circuit that).

The `typeNothing` implicit stays necessary only while the implicits are split by upper bound. A
single `implicit def tpe[T: Tag]` should not be ambiguous for `Nothing`. **Unverified.**

## Implicit macros

An implicit can be a macro on both versions; izumi's `Tag` is one.

```scala
// Scala 2.13 (needs scala-reflect)
implicit def tpe[T]: Type[T] = macro TypeMacro.impl[T]
// Scala 3
implicit inline def tpe[T]: Type[T] = ${ TypeMacro.impl[T] }
```

The two syntaxes have no shared subset, so this needs `src/main/scala-2` and `src/main/scala-3`.
A macro sees the compiler's own subtyping. It can therefore record, for every named type inside
`T` (type arguments included), whether it is AnyVal, AnyRef or a universal trait. That closes the
gap the tag leaves open, and parts built by `combine` carry that marking from their own `Type`.

## The two directions

**A. Keep izumi and add a macro.** The macro records each named type's kind next to the izumi
tag, and our own walk over `LightTypeTagRef` applies the boundary rules with variance. Cost: two
parallel representations kept in sync, with izumi's wrong algorithm still underneath.

**B. Drop izumi and write our own, restricted to what Xi needs** (Eshu's current leaning). One
representation, built by our macro. Shapes outside the subset are **compile errors** instead of
silently wrong answers, which izumi cannot offer. Once A needs a macro and our own walk anyway,
B's remaining work is mostly storing base types ourselves.

### Proposed scope for B

- **Supported:** named classes, traits, objects and Java classes applied to known arguments; the
  top and bottom types (Any, AnyVal, AnyRef, Null, Nothing) and the primitives; type constructors
  of arity 1, 2, 3 as values; aliases dealiased; variance; a `baseType` operation.
- **Compile error:** refinements and structural types, path-dependent types, singleton and literal
  types, intersections (`A with B`, `A & B`), unions, opaque types, type lambdas, existentials with
  bounds, match types. Extend only when Xi needs it.

### Where B costs

1. **Base types with substituted arguments.** Each class needs its ancestors written in terms of
   its own parameters (`List[x] <: Seq[x] <: … <: PartialFunction[Int, x]`, about 20 for `List`),
   substituted on `combine`, so that `List[Int] <: Seq[Any]` works for runtime-built types too.
   Emitted at every call site, that bloats bytecode (izumi packs it into strings for this reason).
   Needs caching per type from the start.
2. **Scala 3 has a different hierarchy.** It has `Matchable` between `Any` and `AnyVal`/`AnyRef`,
   and universal traits extend `Any`, not `Matchable`. `-Yexplicit-nulls` (not enabled now) takes
   `Null` out from under `AnyRef`; write the Null rule so it does not have to be rewritten later.
3. **Two macro implementations, and the 100% coverage gate.** Macro code runs while the tests
   compile, not while they run. Whether scoverage counts it as uncovered is **unverified**; if it
   does, exclude the macro package or test the classification logic apart from the macro glue.
4. **Aliases.** `DataFrame` is `Dataset[Row]`. Dealiased comparison is right, but error messages
   will show `Dataset[Row]`.
5. **`Unlift` checks `in <:< Option[?]`.** Better: `in.baseType(OptionK)`, returning `Option[T]` or
   nothing. That avoids wildcard subtyping and also covers `Some[Int]` as input.
6. **Spark serialization.** If a `Type` ends up in a stage's closure, it has to be serializable to
   ship to executors. Depends on where `StageSpec` is kept; check before designing the
   representation.

## The API (agreed 2026-11-25)

Two things, both summoned by implicit macros:

- **`Type[T]`** with exactly two operations, `<:<` and `=:=`. Nothing else goes on it.
- **`KindN`**, one per arity: a type constructor with N holes, each hole taking a `Type`. It
  both builds and takes apart:

  ```scala
  trait Kind1[F[_]] {
    def apply[A](a: Type[A]): Type[F[A]]
    def unapply(t: Type[?]): Option[Type[?]]
  }
  trait Kind2[F[_, _]] {
    def apply[A, B](a: Type[A], b: Type[B]): Type[F[A, B]]
    def unapply(t: Type[?]): Option[(Type[?], Type[?])]
  }
  ```

Why it has this shape:

- **`unapply` is required, not optional.** `Unlift`, `Coalesce`, `Either.Left` and `Swap` get
  `in: Type[I]` at runtime and have to pull its arguments out. `<:<`/`=:=` plus a constructor
  cannot do that: they check `in =:= Option[X]` for a known `X`, but cannot find `X`. They cannot
  express `in <:< Option[?]` either, since a Kind does not build a wildcard type. `unapply`
  replaces both.
- **`unapply` goes through the base type**, so `Some[Int]` matches `OptionK` and yields `Int`.
  It reuses the base types with substituted arguments that `<:<` needs anyway
  (`List[Int] <: Seq[Any]`), so it costs no new machinery.
- **Not literally `FunctionN`.** `Type[?] => Type[?]` does not say which constructor it is, so
  it cannot be summoned; the Kind has to carry `F`. Do not extend `Function1` either: on 2.13 an
  implicit value of a function type is an implicit view wherever it is in scope (for example in
  a method taking `(implicit k: Kind1[List])`), while Scala 3 only takes `Conversion` as one, so the
  two versions would behave differently.
- **`apply` is a polymorphic method**, so the static type survives (`Type[F[A]]`, not `Type[?]`).
  2.13 has no polymorphic function types, so a method is the only way. Xi erases it anyway; tests
  get it for free.
- **Bounded holes** (`F[A <: AnyRef]`): `apply` could not reject `Type[Int]`. The macro refuses
  such constructors at compile time.
- **Holes are trailing parameters only.** `Either[?, String]` as a `Kind1` would need type lambdas
  (on 2.13 a projection on a refinement). Xi does not need it: `Either.Left` takes `Either` apart
  as a `Kind2`. Compile error until something does.
- **Generic code** gets a Kind the same way: `def f[F[_]: Kind1, A: Type] = Kind1[F].apply(Type[A])`.

**Where comparison and variance live.** `unapply` is for Xi's builders, not for comparing:

- **Comparison is internal to `Type`.** A `Type` is a tree: a constructor and its arguments. The
  constructor's descriptor holds the variance of each parameter and its base types written in terms
  of its own parameters. `<:<` lifts the left side to the right side's constructor through the base
  types, then compares the arguments pairwise according to variance. None of this is public; the
  public surface stays `<:<` and `=:=`.

  For arguments `a` (left) and `b` (right) at the same position:

  | Parameter      | Condition  |
  |----------------|------------|
  | covariant      | `a <:< b`  |
  | contravariant  | `b <:< a`  |
  | invariant      | `a =:= b`  |

  On `Stage[-I, O, +E]`: `Stage[AnyRef, Int] <: Stage[Int, Int]` is `false`, since `I` is
  contravariant and `Int <: AnyRef` is false. `Stage[Any, Int] <: Stage[String, Int]` is `true`, since
  `String <: Any`.
- **Variance is needed when building.** A Kind carries its constructor's descriptor, variances
  included, from the macro. `apply` hangs the arguments on that descriptor. Without the variances, a
  type built by `apply` would compare wrongly (`OptionK(Int) <:< OptionK(Any)` would be `false`).
- **`unapply` needs no variance.** It lifts the type to `F`'s base type and returns that base type's
  arguments as they are: `Some[Int]` → `Option[Int]` → `Int`, `class Foo extends Option[String]` →
  `String`.

**Hierarchy differences between 2.13 and 3.** Without `-Yexplicit-nulls`, `Null` behaves the same on
both: a subtype of everything that is not `AnyVal`, universal traits included (both compilers accept
`val x: UniversalParentTrait = null`). The difference today is `Matchable` on 3, which universal
traits do not extend. `Null` changes only under explicit nulls. Keep the hierarchy rules per Scala
version, filled in by the macro, so that either can change without touching the other.

## Open decisions (Eshu's)

- **A or B.** Leaning B. The API above holds for either.
- **`Null <: universal trait`:** Scala says `true`, `TypeNull` says `false` (`TypeNullTest` asserts
  it on purpose). With A and Scala's Null rule the answer is forced to `true`. With a macro, either
  is possible. Decide before sketching.
- **`Matchable` on Scala 3:** expose it, or hide it so that both versions report the same hierarchy.
- **`unapply` on the bottom types.** `Nothing <: Option[X]` holds for every `X`, and
  `Null <: Option[X]` too (`Option` is an AnyRef), but neither has a base type `Option[…]` to take
  apart. Proposal: `OptionK.unapply(Nothing)` is `Some(Nothing)`, consistent with `Option[Nothing]`,
  so that `Unlift` outputs `Nothing`, which is sound. `OptionK.unapply(Null)` is `None`, since `null` is
  not an `Option` and there is nothing to take apart.
## Next step, when this is picked up

A sketch on 2.13 and 3.9: implicit macros producing `Type[Stage[-I, O, +E]]` with base types,
`Kind1[Option]` and `Kind2[Tuple2]`, and the comparison. Check:

- `Stage[AnyRef, Int] <: Stage[Int, Int]` is `false`; `Stage[Any, Int] <: Stage[String, Int]` is `true`;
- `OptionK.unapply(Type[Some[Int]])` gives `Int`; `OptionK.unapply(Type[List[Int]])` gives nothing;
- `Kind2[Tuple2].apply(Type[Int], Type[String]) =:= Type[(Int, String)]`;
- what scoverage reports for the macro code;
- the generated code size for `List[Int]`.

## Appendix: probes

Run with `scala-cli run -S 2.13 Probe.scala` and `-S 3.9.0`. The fixtures are local
`trait UP extends Any`, `trait UT extends Any with UP`,
`final class VC(val x: Int) extends AnyVal with UT`, `trait RT`, `class RC extends RT`.

```scala
//> using dep dev.zio::izumi-reflect:3.0.11
package izumi.reflect.probe // only so that the private[reflect] idb can be read

import izumi.reflect.{Tag, TagK, TagKK}

object Probe {
  type Id[A] = A
  def gen[F[_]: TagK, A: Tag]: Tag[F[A]] = Tag[F[A]]
  def gen2[F[_, _]: TagKK, A: Tag, B: Tag]: Tag[F[A, B]] = Tag[F[A, B]]

  def main(args: Array[String]): Unit = {
    println(gen[List, Int].tag =:= Tag[List[Int]].tag)
    println(TagK[List].tag.combine(Tag[String].tag) <:< Tag[Seq[CharSequence]].tag)
    println(TagKK[Either].tag.combine(Tag[String].tag)) // λ %1 → Either[+String,+1]
    println(Tag.appliedTag(TagK[Id], List(Tag[VC].tag)).tag <:< Tag[AnyVal].tag)
    println(Tag[UT].tag.idb) // only universal parents, no Any
    println(Tag[Nothing].tag <:< Tag[String].tag)
  }
}
```
