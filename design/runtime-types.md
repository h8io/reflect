# Runtime types: what reflect should become

Status: **direction decided (B), API agreed, first implementation in place** (2026-11-27, see
"Results of the sketch"). Written 2026-11-25.

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

### What izumi gets wrong

Raw `LightTypeTag <:<` answers against the current `Type` and Scala:

| Pair                       | izumi | `Type` | Scala |
|----------------------------|-------|--------|-------|
| `Int <: AnyRef`            | true  | false  | false |
| `ValClass <: AnyRef`       | true  | false  | false |
| `UniversalTrait <: AnyRef` | true  | false  | false |
| `Null <: Int`              | true  | false  | false |
| `Null <: ValClass`         | true  | false  | false |

To izumi, `AnyRef` is a supertype of nearly everything and `Null` a subtype of everything, as if it
were `Nothing`. Everything else probed agrees with Scala: primitives and their Java boxes are
unrelated, `String` is not an `AnyVal`, variance works, `Object =:= AnyRef`.

The current `Type` corrects these only at the top level, by deciding the AnyVal/AnyRef boundary from
the upper bound of `T` when the implicit is picked. Type arguments are compared by izumi, so its
mistakes leak through: `Type` answers `true` for `List[Int] <: List[AnyRef]`,
`List[ValClass] <: List[AnyRef]`, `Option[UniversalTrait] <: Option[AnyRef]` and
`List[Null] <: List[Int]`, all of which Scala rejects.

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
  `true`, which is the opposite of what `TypeNull` said (decided: `true`, see "The rule").
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

**B. Drop izumi and write our own, restricted to what Xi needs** (**decided 2026-11-25**). One
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
   compile, not while they run. Resolved: scoverage records it there, so it is covered by the tests
   that compile it (see "Results of the sketch").
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

## The rule: the compiler is the oracle (decided 2026-11-27)

`Type[A] <:< Type[B]` is `true` exactly when the compiler proves `A <: B`, that is, when
`implicitly[A <:< B]` compiles; likewise `=:=` against `implicitly[A =:= B]`, on the Scala version
in use. If the compiler can glue `Stage[?, O, ?] ~> Stage[I, ?, ?]`, then `Type[O] <:< Type[I]`.
No deviations in either direction, at any depth.

- **Subtyping, not assignability.** `val x: Long = 1: Int` compiles through numeric widening (and
  any implicit conversion), but `Int <: Long` is false. Only `<:<` evidence counts.
- **Per version.** Where 2.13 and 3 differ (`Matchable`, later explicit nulls), `Type` differs the
  same way.
- **Tests take the expected answer from the compiler**, not from a hand-written table: ScalaTest's
  `assertCompiles("implicitly[A <:< B]")` / `assertTypeError(...)` work on both versions, so each
  fixture pair can be checked against the compiler and the runtime answer side by side.

Consequence: **`Null <: universal trait` is `true`**, since both compilers accept
`val x: UniversalParentTrait = null`. The old `TypeNull` answer (`false`) is dropped with the old code.

**`Matchable` is exposed on Scala 3** (decided 2026-11-27). Hiding it by mapping it to `Any` would
break the rule (`UniversalTrait <: Matchable` would come out `true`, Scala 3 says `false`). On 3,
`Type[Matchable]` is a real type between `Any` and `AnyVal`/`AnyRef`; on 2.13 it does not exist.

## `unapply` (decided 2026-11-27)

**Ordinary types:** `KindN[F].unapply(t)` takes the base type of `t` with constructor `F`, as the
compiler computes it, and returns its arguments: `Some[Int]` → `Option[Int]` → `Int`,
`None.type` → `Option[Nothing]` → `Nothing`, `List[Int]` → nothing.

**Bottom types.** `Nothing <: F[X]` and `Null <: F[X]` (for an AnyRef `F`) hold for every `X`, so
there is no base type to take apart. `unapply` returns the arguments that make `F[args]` the least
type: `Nothing` for a covariant hole, `Any` for a contravariant one, and by convention `Nothing` for
an invariant one, which has no least (`In[Nothing]` and `In[Any]` are unrelated). `Null` and
`Nothing` get the same answer: `OptionK.unapply(Null)` is `Some(Nothing)`, so `Unlift` on `null`
outputs `Nothing`, which is sound, since it never returns normally. `Kind3[Stage]` on either gives
`(Any, Nothing, Nothing)`. The answer satisfies the rule (`t <: F[args]` holds), is the same on both
versions, and `Nothing` in an output glues to any next stage.

"Whatever the compiler infers" was considered and rejected: inference depends on the call site,
not on `t`, and differs between versions. Measured for an unconstrained parameter (input `Nothing`
or `null`, same answers):

| Signature | 2.13 | 3.9 |
|---|---|---|
| `f[A](x: Co[A]): Unit` | `Nothing` | `Any` |
| `f[A](x: In[A]): Unit` | `Nothing` | `Any` |
| `f[A](x: Contra[A]): Unit` | `Any` | `Any` |
| `f[A](x: Contra[A]): A` | `Any` | `Nothing` |
| `unlift[A](x: Option[A]): A` | `Nothing` | `Nothing` |
| `stO[I, O, E](x: St[I, O, E]): O` | `Any, Nothing, Nothing` | `Any, Nothing, Any` |

2.13 always picks `Nothing` for co- and invariant parameters and `Any` for contravariant ones; 3.9
goes by the variance in the result type, and picks `Any` when the parameter does not occur there.

**Two instances of one generic ancestor.** Both versions allow a class to inherit, say, `Box[Any]`
and `Box[Int]` for a variant parameter; neither allows it for an invariant one. Measured base types:

| Ancestors | 2.13 | 3.9 |
|---|---|---|
| `Box[+T]`: `Box[Any]`, `Box[Int]` | `Box[Int]` | `Box[Int]` |
| `CBox[-T]`: `CBox[Int]`, `CBox[Any]` | `CBox[Any]` | `CBox[Any]` |
| `CBox[-T]`: `CBox[Int]`, `CBox[String]` | illegal inheritance | `CBox[Int \| String]` |

2.13 accepts it only when one instance conforms to the other and keeps the narrower; 3.9 always
accepts it and merges the arguments (intersection for covariant, union for contravariant). The
macro takes the compiler's `baseType` as it is. If that lies in the supported subset, it is used,
and both versions agree; if it is a union or intersection (possible only on 3), it is a compile
error like any other type outside the subset. No separate check is needed.

## Rewrite from scratch (decided 2026-11-25)

The existing code may be dropped entirely; nothing of the current `Type` has to survive.

- **Kept:** the build skeleton (`build.sbt`, `project/`, `test.sh`, the workflows) and the test
  fixtures in `h8io.reflect.test` (`UniversalParentTrait`, `UniversalTrait`, `RefTrait`, `ValClass`,
  `Primitives`). They are exactly the hard cases where izumi broke, and the new code needs them too.
- **Dropped:** `Type.scala` and the tests of its cases (`Type<Case>Test`, `PrimitivesTest`,
  `TypeTest`). What they encoded is in this document and in git history.
- **In one PR with the sketch below, not separately.** A `main` with an empty `src/main` would let
  any `vX.Y.Z` tag publish an empty library. Xi is pinned to 0.0.6, so nothing breaks at once, but
  there is no reason to open that window.
- **The same PR also changes the skeleton:** `src/main/scala-2` and `src/main/scala-3` for the
  macros, `scala-reflect` (provided) on 2.13, `izumi-reflect` removed from `Dependencies.scala`.
  AGENTS.md forbids dependency changes as a side effect, so it has to be stated in the PR.
- **AGENTS.md gets a description of the new code in the same PR.** The description of the old
  `Type` was removed ahead of the rewrite; the izumi findings moved here ("What izumi gets wrong").

## Results of the sketch (2026-11-27)

Implemented on 2.13 and 3.9 as described above; `izumi-reflect` is gone. Every item of the former
"Next step" holds, each runtime answer checked against the compiler's in the tests:

- `Stage[AnyRef, Int, Nothing] <: Stage[Int, Int, Nothing]` is `false`;
  `Stage[Any, Int, Nothing] <: Stage[String, Int, Nothing]` is `true`. (`Stage` has three
  parameters; the two-argument forms used earlier in this document were shorthand.)
- `Kind1[Option].unapply` gives `Int` for `Some[Int]`, `Nothing` for `None.type`, nothing for
  `List[Int]`. `Kind2[Tuple2].apply(Type[Int], Type[String]) =:= Type[(Int, String)]`.
- On `Nothing` and `Null`, `Kind1[Option].unapply` gives `Nothing` and `Kind3[Stage]` gives
  `(Any, Nothing, Nothing)`; on `Null` an AnyVal constructor (`ValBox[A] extends AnyVal`) gives
  nothing, since `Null <: ValBox[X]` is false for every `X`.
- On 3, a class whose base type is `CBox[Int | String]` is a compile error, naming the base type.

What was found on the way:

- **scoverage measures macro code.** The macros run in the compiler while the tests compile, and the
  instrumented macro classes record their statements there like any other code. No exclusion is
  needed; branches that end in a compile error are covered by `assertTypeError` snippets. Branches
  that no input reaches showed up as uncovered and were removed: on 3 a type constructor always
  arrives eta-expanded (`[A] =>> Option[A]`, never a bare `Option`), `AppliedType` is always fully
  applied, and `dealias` drops annotations on both versions. One gap: on 2.13 scoverage's default
  `excludedSymbols` leave out everything typed `Tree`, so the entry points `tpe` and `kind` are not
  measured there; the `Encoder`, where the classification happens, is.
- **Bounded and higher-kinded holes need no check of their own.** Both compilers refuse
  `Kind1[Bounded]` (`Bounded[A <: AnyRef]`) and `Kind1[Fix]` (`Fix[F[_]]`) as arguments for
  `F[_]` before the macro runs.
- **Base types only need class names at their head.** `<:<` lifts the left side to the right side's
  class and compares arguments with the right side's variances, so a base type stores the class
  name and its arguments; only argument classes carry a full description. Base types whose class
  has higher-kinded parameters (`IterableOps[A, List, List[A]]`) are dropped: no `Type` can name
  them, so they can never be the right side. A base type outside the subset in any other way is a
  compile error, since it could be.
- **Objects are supported** as the class of their single instance (`None.type <: Option[Int]`).
  On 2.13 an object's name gets a `.type` suffix, since it shares its full name with its companion
  class; on 3 the module class name (`None$`) is distinct already.
- **Generated code size.** For `Type[List[Int]]` the table has two classes (`List` with about 20 base
  types, `Int` with its 2 or 3) and costs about 3 KB of bytecode over an empty object on 2.13 and
  about 4 KB on 3.9; `Type[Int]` about 1.3 KB and 1.8 KB, `Kind1[Option]` about 1.9 KB and 2.5 KB.
  Fine for a sketch; no caching yet. If Xi summons many types, the next step is a string encoding
  decoded once per class (what izumi does).
- On 3.9, `List`'s base classes include `scala.caps.Pure`, from the capture-checked library.

Still open:

- **Spark serialization** (item 6 under "Where B costs"): `Type` and `Kind` are not `Serializable`.
  Depends on where Xi keeps `StageSpec`.
- **Caching** of class descriptions across call sites, see the code size above.

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
