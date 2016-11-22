import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._

ThisBuildDefault.settings

organization in ThisBuild := "edu.umass.cs.iesl"

// autoCompilerPlugins  := true

val libV = LibVersions
val Lib = CommonLibs


val commonDeps = LogLibs.logback ++
  TestLibs.testAndCheck ++
  Lib.matryoshkaLibs ++ Seq(
    // Lib.scalazCore,
    Lib.sourcecode,
    Lib.aspectJ
    // compilerPlugin("org.spire-math" %% "kind-projector"  % "0.9.2")
  )


import ReleaseTransformations._

lazy val root = (project in file("."))
  .aggregate(examples)


lazy val examples = (project in file("examples"))
  .settings(AspectJ.settings: _*)
  .settings(
    addCompilerPlugin("org.spire-math" %% "kind-projector"   % "0.9.2"),
    addCompilerPlugin("org.scalamacros" % "paradise"         % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("com.milessabin"  % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
  )
  .settings(libraryDependencies ++= commonDeps)
