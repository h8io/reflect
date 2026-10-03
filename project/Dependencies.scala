import h8io.sbt.dependencies.*
import sbt.*

object Dependencies {
  val IzumiReflect = "dev.zio" %% "izumi-reflect" % "3.0.11"

  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.20",
      "org.scalamock" %% "scalamock-scalatest" % "7.6.0"
    )
}
