name := "SpinalWishbone"

version := "1.0"

scalaVersion := "2.12.0"

//EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "com.github.spinalhdl" %% "spinalhdl-core" % "1.1.3",
  "com.github.spinalhdl" %% "spinalhdl-lib" % "1.1.3"
)

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.0" % "1.0.3")

scalacOptions += "-P:continuations:enable"
scalacOptions += "-deprecation"
scalacOptions += "-unchecked"
fork := true
