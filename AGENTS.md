# AGENTS.md

Guidance for coding agents working in this repository.

## What this is

`io.h8 %% reflect`: a small reflection library for Scala. It exists to serve the needs of
[Xi](https://github.com/h8io/xi), so its API grows only as far as Xi needs it.

**The library is being rewritten from scratch.** The current `Type.scala` (built on
izumi-reflect) is to be replaced by an own macro-based representation without izumi. Do not
extend the current code. The design, the agreed API, the measurements behind it and the open
decisions are in `design/runtime-types.md`; read it before touching `src/main`.

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
- Source must compile on Scala 2.13 and 3 alike. Scala 2 runs with `-Xsource:3`, so use
  the shared subset: `?` for wildcard types, `import a.{b as c}` / `import java.util as ju`,
  `implicit` (not `given`/`using`), no Scala-3-only syntax such as braceless blocks or enums.

## Tests

ScalaTest `AnyFlatSpec with Matchers` under `src/test/scala/h8io/reflect`. The fixtures in the
`h8io.reflect.test` package are kept across the rewrite: a universal trait hierarchy
(`UniversalParentTrait`, `UniversalTrait`), `RefTrait`, the value class `ValClass`, and
`Primitives`, the list of all primitive cases. They are the cases izumi got wrong.

## Workflow

- Never commit to `main`. Branch, commit, push, open a PR against `main`.
- CI (`.github/workflows/test.yaml`) runs `test.sh` through the reusable workflows in
  `h8io/gha` and posts the coverage summary on the PR.
- Releases are cut by pushing a `vX.Y.Z` tag; the version comes from git via sbt-dynver
  (`versionScheme` is `semver-spec`).
