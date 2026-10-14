import Dependencies.*
import h8io.sbt.dependencies.*
import sbt.*

val ProjectName = "reflect"

// Every branch on the Scala version lists the versions it knows and ends here, so that a new one fails the build until
// each of them has been looked at
def unsupportedScala(version: String): Nothing = throw new MessageOnlyException(s"Unsupported Scala version $version")

organization := "io.h8"
organizationName := "H8IO"
organizationHomepage := Some(uri(s"https://github.com/h8io/$ProjectName"))
homepage := Some(uri(s"https://github.com/h8io/$ProjectName"))
scmInfo := Some(ScmInfo(uri(s"https://github.com/h8io/$ProjectName"), s"scm:git@github.com:h8io/$ProjectName.git"))
licenses := List(License.Apache2)
developers := List(
  Developer(id = "eshu", name = "Pavel", email = "tjano.xibalba@gmail.com", url = uri("https://github.com/eshu/")))

versionScheme := Some("semver-spec")

coverageSummaryStmtLowThreshold := 100
coverageSummaryStmtHighThreshold := 100
coverageSummaryBranchLowThreshold := 100
coverageSummaryBranchHighThreshold := 100

// sbt-dynver computes the version once for the whole build, from these two in ThisBuild
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / dynverSeparator := "-"

scalacOptions ++= {
  val scala2 = Seq("-Xsource:3", "--deprecation", "--feature", "--unchecked", "-Xlint:_", "-Xfatal-warnings",
    "-opt:l:inline", "-opt-warnings")
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) =>
      scala2 ++ Seq("--explain-types", "--language:_", "-Wunused:_", "-Wdead-code", "-Wvalue-discard",
        "-Wnonunit-statement")
    case Some((2, 12)) =>
      scala2 ++ Seq("-language:higherKinds", "-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-unused:-nowarn",
        "-Ywarn-value-discard", "-Ypartial-unification")
    case Some((3, 9)) =>
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Wall",
        "-Werror",
        "-explain",
        "-opt",
        "-Wopt:at-inline-failed-summary",
        // Scala 2 needs the trailing `_` to eta-expand a method without an expected function type, Scala 3 does not
        "-Wconf:msg=eta-expansion is unnecessary:s"
      )
    case _ => unsupportedScala(scalaVersion.value)
  }
}

// For code whose statements are values on purpose: a ScalaMock expectation or a ScalaTest assertion in tests. Those
// are spared the warnings about discarded values. On 2.13 the category of -Wnonunit-statement is shared with the
// warning about a pure expression in statement position, which they lose as well.
def lenientScalacOptions(scalaVersion: String): Seq[String] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => Seq("-Wconf:cat=w-flag-value-discard:s,cat=other-pure-statement:s")
    case Some((2, 12)) => Seq("-Wconf:cat=w-flag-value-discard:s")
    case Some((3, 9)) => Seq("-Wconf:name=ValueDiscarding:s,name=UnusedNonUnitValue:s")
    case _ => unsupportedScala(scalaVersion)
  }

Test / scalacOptions ++= lenientScalacOptions(scalaVersion.value)

javacOptions ++= Seq("-target", "8")

scalaVersion := "2.13.18"
crossScalaVersions ++= Seq("2.12.21", "3.9.0")

libraryDependencies ++= TestBundle % Test

val root = (project in file("."))
  .enablePlugins(ScoverageSummaryPlugin)
  .settings(
    name := ProjectName,
    libraryDependencies += IzumiReflect
  )
