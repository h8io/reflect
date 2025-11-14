import h8io.sbt.dependencies.*
import sbt.*

object Dependencies {
  val ScalaReflect212 = "org.scala-lang" % "scala-reflect" % "2.12.20"
  val ScalaReflect213 = "org.scala-lang" % "scala-reflect" % "2.13.17"

  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.19",
      "org.scalamock" %% "scalamock" % "7.5.2"
    )
}
