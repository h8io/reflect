# AGENTS.md

Guidance for coding agents working in this repository.

## What this is

`io.h8 %% reflect`: a small reflection library for Scala, built on
[izumi-reflect](https://github.com/zio/izumi-reflect). It exists to serve the needs of
[Xi](https://github.com/h8io/xi), so its API grows only as far as Xi needs it.

The whole library is `src/main/scala/h8io/reflect/Type.scala`:

- `sealed trait Type[T]` with two operations: `<:<` (subtype) and `=:=` (type equality).
- Private singletons for the top and bottom types: `TypeAny`, `TypeAnyVal`, `TypeAnyRef`,
  `TypeNothing`, `TypeNull`.
- `Primitive[T, B]` for the nine primitives (`TypeBoolean` … `TypeUnit`), each carrying the
  `Type` of its boxed Java class. A primitive and its box are unrelated: `Int <:< Integer`
  is `false` both ways.
- Three wrappers around an izumi `LightTypeTag`, chosen by the upper bound of `T`:
  `TypeVal` (`T <: AnyVal`), `TypeRef` (`T <: AnyRef`) and `TypeAnyLike` (anything else,
  e.g. universal traits extending `Any`). Comparisons across wrappers are deliberately
  asymmetric: a `TypeVal` or `TypeRef` can be a subtype of a `TypeAnyLike`, never the
  other way round, and `TypeNull` is a subtype of `TypeRef` but not of `TypeAnyLike`.
- `object Type` holds the implicit instances. Which one is picked for a given `T` relies
  on Scala's overloading specificity among implicits in the same scope (a concrete
  `typeInt` beats `typeVal`, `typeVal`/`typeRef` beat `typeAnyLike`). Adding or changing an
  implicit can silently move a type into a different case; the `shouldBe a[TypeXxx[?]]`
  tests are what catch that.

Every concrete case is `private`; only `Type`, `Primitive` and the implicits in
`object Type` are public. Keep it that way unless a change in Xi needs more.

## Build

- sbt **2** (`project/build.properties`), JDK 17 in CI.
- Cross build: Scala **2.13** (default `scalaVersion`), **2.12** and **3.9**. Use `+` to run
  a command on all of them; a change is not done until it compiles and passes on every row.
- Every match on the Scala version in `build.sbt` ends in `unsupportedScala`, so adding a
  version fails the build until each branch has been extended. Do not add a catch-all.
- Warnings are errors on every version (`-Xfatal-warnings` / `-Werror`, plus `-Xlint`,
  `-Wunused`, `-Wall`, etc.). Tests get `lenientScalacOptions`, which only silences
  discarded-value warnings (ScalaTest assertions and ScalaMock expectations are values used
  as statements).
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
sbt scalafmtAll scalafmtSbt
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
fallbacks in pattern matches. Keep `+coverageSummary` and `+coverageAggregate` ahead of
`+coverageSummaryCheck` in `test.sh`: `+` runs each command over all rows before the next,
so a check placed earlier would stop the build before the other rows' reports are written.

## Code style

- `scalafmt` (`.scalafmt.conf`, 120 columns) is checked in CI; run `sbt scalafmtAll scalafmtSbt`
  before committing.
- Source must compile on Scala 2.12, 2.13 and 3 alike. Scala 2 runs with `-Xsource:3`, so use
  the shared subset: `?` for wildcard types, `import a.{b as c}` / `import java.util as ju`,
  `implicit` (not `given`/`using`), no Scala-3-only syntax such as braceless blocks or enums.

## Tests

ScalaTest `AnyFlatSpec with Matchers` under `src/test/scala/h8io/reflect`:

- one `Type<Case>Test` per case class/object (`TypeAnyTest`, `TypeRefTest`, …) checking its
  runtime class, `toString`, and `<:<`/`=:=` against every other case;
- `PrimitivesTest` for the primitives, `TypeTest` for end-to-end checks on real types,
  including variance of type parameters;
- fixtures in the `h8io.reflect.test` package: a universal trait hierarchy
  (`UniversalParentTrait`, `UniversalTrait`), `RefTrait`, the value class `ValClass`, and
  `Primitives`, the list of all primitive cases.

The `toString` block at the end of `TypeTest` has been commented out since the move from
scala-reflect to izumi-reflect (#14); its expectations were written for scala-reflect's
rendering. Don't re-enable it without checking what `scalaStyledRepr` gives on every row of
the cross build.

## Workflow

- Never commit to `main`. Branch, commit, push, open a PR against `main`.
- CI (`.github/workflows/test.yaml`) runs `test.sh` through the reusable workflows in
  `h8io/gha` and posts the coverage summary on the PR.
- Releases are cut by pushing a `vX.Y.Z` tag; the version comes from git via sbt-dynver
  (`versionScheme` is `semver-spec`).
