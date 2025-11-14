import h8io.sbt.dependencies.*
import sbt.*

object Dependencies {
  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.19",
      "org.scalamock" %% "scalamock" % "7.5.0"
    )
}
