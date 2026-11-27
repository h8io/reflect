# AGENTS.md

Guidance for coding agents working in this repository.

## What this is

`io.h8 %% reflect`: a small reflection library for Scala. It exists to serve the needs of
[Xi](https://github.com/h8io/xi), so its API grows only as far as Xi needs it.

The design, the agreed API, the measurements behind it and the decisions are in
`design/runtime-types.md`; read it before touching `src/main`.

## The code

The public API is two things, both summoned by implicit macros:

- `Type[T]` (`Type[List[Int]]`) with `<:<` and `=:=` only. They answer exactly what the compiler
  proves on the Scala version in use: `Type[A] <:< Type[B]` is `true` iff `implicitly[A <:< B]`
  compiles. No deviations in either direction, at any depth.
- `Kind1[F]`, `Kind2[F]`, `Kind3[F]`: a class used as a type constructor. `apply` builds
  `Type[F[A]]` from `Type[A]`; `unapply` takes a type apart through its base type with class `F`
  (`Kind1[Option].unapply(Type[Some[Int]])` gives `Int`).

Layout:

- `src/main/scala/h8io/reflect`: `Type`, `Kind*`. `internal/Repr.scala` is the runtime model: a
  `Repr` is `Nothing`, `Null` or a `Ctor` applied to arguments; a `Ctor` holds its fully qualified
  name (the identity), variances, whether `Null` conforms to it, and every base type keyed by class
  name, written over its own parameters (`Tpl`). `<:<` lifts the left side to the right side's class
  through the base types and compares the arguments by variance. `internal/Spec.scala` is what the
  macros emit (a table of classes referring to each other by index, so a class can occur in its own
  base types); it is public only because expanded code lives at the call site.
- `src/main/scala-2` and `src/main/scala-3`: the macros, one implementation per version, since the
  two macro APIs share nothing. They walk the type, describe each class with the compiler's own
  answers (base types, variance, `Null <: C`) and emit `Spec` data. Everything outside the
  supported subset is a compile error with a reason, including a base type outside it (on Scala 3 a
  class can get `CBox[Int | String]`). Base types whose class has higher-kinded parameters
  (`IterableOps[A, List, List[A]]`) are left out: no `Type` can be built for them.

Supported: static classes, traits, objects and Java classes applied to supported arguments, `Any`,
`AnyVal`, `AnyRef`, `Null`, `Nothing`, primitives, `Matchable` on Scala 3. Compile errors:
refinements, intersections, unions, literal types, local and path-dependent classes, abstract and
opaque types, higher-kinded arguments, wildcards. Generic code builds types with `Kind*.apply`, not
`Type[F[A]]`. Extend the subset only when Xi needs it.

## Build

- sbt **2** (`project/build.properties`), JDK 17 in CI.
- Cross build: Scala **2.13** (default `scalaVersion`) and **3.9**. Use `+` to run
  a command on both; a change is not done until it compiles and passes on every row.
- Every match on the Scala version in `build.sbt` ends in `unsupportedScala`, so adding a
  version fails the build until each branch has been extended. Do not add a catch-all.
- Warnings are errors on every version (`-Xfatal-warnings` / `-Werror`, plus `-Xlint`,
  `-Wunused`, `-Wall`, etc.). Tests get `lenientScalacOptions`, which only silences
  discarded-value warnings (ScalaTest assertions and ScalaMock expectations are values used
  as statements).
- Scala 2.13 needs `scala-reflect` (`Provided`) for its macros; it is added in `build.sbt`, not in
  `Dependencies.scala`, since it follows `scalaVersion`.
- Settings are written bare, not under `ThisBuild`, except sbt-dynver's keys, which sbt-dynver
  reads from `ThisBuild` only.
- Dependencies live in `project/Dependencies.scala`. Version bumps usually arrive from the
  `h8io[bot]` account; don't bump versions as a side effect of other work.

### Commands

```
./test.sh        # what CI runs: format check, clean build, coverage on all Scala versions, docs, packaging
sbt +testFull    # tests on all Scala versions
sbt testFull     # tests on 2.13 only, fastest loop
sbt "++3.9.0 testFull"
sbt +scalafmtAll scalafmtSbt
```

sbt 2 specifics that matter here:

- `test` is incremental and may skip suites whose inputs did not change; use `testFull` when
  you need everything to actually run.
- `cleanFull` (not `clean`) is needed before coverage: `clean` keeps sbt's machine-wide disk
  cache, and a compilation restored from it bypasses the compiler and therefore scoverage,
  so coverage data ends up missing.

`release.sh` and `snapshot.sh` publish via `sbt-ci-release`; they are run by the GitHub
workflows, never locally.

## Coverage gate: 100%

`build.sbt` sets statement and branch thresholds to 100, and `test.sh` ends with
`+coverageSummaryCheck`, so **any uncovered statement or branch fails CI** on any Scala
version. Every code change needs tests that cover each new branch, including the `case _`
fallbacks in pattern matches.

The macro code counts too, and it is measured: the macros run in the compiler while the tests
compile, and scoverage records that. Branches of a macro that end in a compile error are covered by
snippets in `assertTypeError`, which run the macro the same way. A branch no input can reach has to
go rather than be excluded; check the line numbers in `scoverage.xml` before concluding that. On
2.13 scoverage's default `excludedSymbols` leave out everything typed `Tree`, so the macro entry
points there (`tpe`, `kind`) are not measured at all; only the `Encoder` is.

Keep `+coverageSummary` and `+coverageAggregate` ahead of `+coverageSummaryCheck` in `test.sh`: `+`
runs each command over all rows before the next, so a check placed earlier would stop the build
before the other rows' reports are written.

## Code style

- `scalafmt` (`.scalafmt.conf`, 120 columns) is checked in CI on both versions; run
  `sbt +scalafmtAll scalafmtSbt` before committing. A plain `scalafmtAll` skips the sources of the
  other version's directory; `src/main/scala-3` and `src/test/scala-3` are formatted as Scala 3.
- Shared source must compile on Scala 2.13 and 3 alike. Scala 2 runs with `-Xsource:3`, so use
  the shared subset: `?` for wildcard types, `import a.{b as c}` / `import java.util as ju`,
  `implicit` (not `given`/`using`), no Scala-3-only syntax such as braceless blocks or enums.

## Tests

ScalaTest `AnyFlatSpec with Matchers` under `src/test/scala/h8io/reflect`, with `src/test/scala-3`
for what exists only on Scala 3 (`Matchable`, unions). **The compiler is the expected answer:** next
to every runtime check sits the compiler's own, `assertCompiles("implicitly[A <:< B]")` or
`assertTypeError(...)`, never a hand-written table. Fixtures are in the `h8io.reflect.test`
package: the universal trait hierarchy (`UniversalParentTrait`, `UniversalTrait`), `RefTrait`, the
value class `ValClass` and `Primitives` (the cases izumi got wrong), plus `Stage`, variant traits and
classes with unusual base types in `Variant.scala`.

A test of a compile error passes for any error, so check once that the error is the macro's: put the
snippet in a scratch source and read the message (`… is not supported by h8io.reflect: …`). A class
the compiler itself rejects (Scala 3 refuses `extends CBox[Int] with CBox[String]` directly) makes
such a test pass for the wrong reason.

## Workflow

- Never commit to `main`. Branch, commit, push, open a PR against `main`.
- CI (`.github/workflows/test.yaml`) runs `test.sh` through the reusable workflows in
  `h8io/gha` and posts the coverage summary on the PR.
- Releases are cut by pushing a `vX.Y.Z` tag; the version comes from git via sbt-dynver
  (`versionScheme` is `semver-spec`).
