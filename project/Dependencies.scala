import h8io.sbt.dependencies.*
import sbt.*

object Dependencies {
  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.20",
      "org.scalamock" %% "scalamock-scalatest" % "7.6.3"
    )
}
