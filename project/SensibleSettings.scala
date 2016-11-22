import scala.util.{ Properties, Try }
import sbt._
import Keys._

// import com.github.fedragon.todolist.TodoListPlugin.autoImport._
import com.lihaoyi.workbench.Plugin._

trait LibVersions {
  val scalazVersion       = "7.2.7"
  val scalaTagsVersion    = "0.6.2"
  val scalaAsyncVersion   = "0.9.6"
  val scalaModulesVersion = "1.0.4"
  val akkaVersion         = "2.3.14"
  val streamsVersion      = "1.0"
  val scalatestVersion    = "3.0.1"
  val logbackVersion      = "1.7.21"
  val quasiquotesVersion  = "2.0.1"
  val guavaVersion        = "18.0"
  val specs2Version       = "3.7"
  val scrimageVersion     = "2.1.7"
  val monocleVersion      = "1.3.2"
  val aspectjVersion      = "1.8.9"
}

object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatest = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
    // "org.scalactic" %% "scalactic" % scalatestVersion
  )

  val scalacheck = Seq(
    "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion  % "test",
    "org.scalacheck" %% "scalacheck"                % "1.13.4"       % "test" force()
  )

  val testAndCheck = scalatest ++ scalacheck
}

object LogLibs extends LibVersions {
  val logback = Seq(
    "org.log4s"      %% "log4s"            % "1.3.3",
    "ch.qos.logback"  % "logback-classic"  % "1.1.7",
    "org.slf4j"       % "slf4j-api"        % logbackVersion,
    "org.slf4j"       % "jul-to-slf4j"     % logbackVersion,
    "org.slf4j"       % "jcl-over-slf4j"   % logbackVersion
  )
}

object DatabaseLibs extends LibVersions {
  val slickDb = Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.55",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.55",
    "com.h2database" % "h2" % "1.4.192",
    "com.zaxxer" % "HikariCP" % "2.5.1",
    "com.typesafe.slick" %% "slick" % "3.1.1"
  )

}
object CommonLibs extends LibVersions {

  val scalazCore       = "org.scalaz"              %% "scalaz-core"      % scalazVersion
  val scalaAsync       = "org.scala-lang.modules"  %% "scala-async"      % scalaAsyncVersion
  val scalatags        = "com.lihaoyi"             %% "scalatags"        % scalaTagsVersion
  val ammonite         = "com.lihaoyi"              % "ammonite"         % "0.8.0" cross CrossVersion.full
  val fastparse        = "com.lihaoyi"             %% "fastparse"        % "0.4.2"
  val sourcecode       = "com.lihaoyi"             %% "sourcecode"       % "0.1.2"
  val playJson         = "com.typesafe.play"       %% "play-json"        % "2.5.9"
  val scopt            = "com.github.scopt"        %% "scopt"            % "3.5.0"
  val machinist        = "org.typelevel"           %% "machinist"        % "0.6.1"
  val shapeless        = "com.chuusai"             %% "shapeless"        % "2.3.2"
  val aspectJ          = "org.aspectj"              % "aspectjweaver"    % aspectjVersion

  val matryoshkaCore   = "com.slamdata"            %% "matryoshka-core"  % "0.11.1"
  // Needed for matryoshka, not needed when I properly build it (rather than putting in ./lib dir)
  val matryoshkaLibs = Seq(
    "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
    "org.scalaz"                 %% "scalaz-core"  % scalazVersion,
    "com.github.mpilquist"       %% "simulacrum"   % "0.10.0"
  )


}

object ThisBuildDefault {
  def colorPrompt = { s: State =>
    val c = scala.Console
    val blue = c.RESET + c.BLUE + c.BOLD
    val white = c.RESET + c.BOLD
    val projectName = Project.extract(s).currentProject.id

    "[" + blue + projectName + white + "]>> " + c.RESET
  }


  lazy val settings =  Seq(

    resolvers in ThisBuild ++= List(
      "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
      // "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.jcenterRepo
    ),

    scalaVersion in ThisBuild := "2.11.8",

    shellPrompt in ThisBuild := colorPrompt,

    autoCompilerPlugins in ThisBuild := true,

    ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet,

    // addCompilerPlugin("org.spire-math" %% "kind-projector"   % "0.9.2"),
    // addCompilerPlugin("org.scalamacros" % "paradise"         % "2.1.0" cross CrossVersion.full),
    // addCompilerPlugin("com.milessabin"  % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full),

    scalacOptions in ThisBuild ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-unchecked",
      // "-Xfatal-warnings",
      "-Xfuture",
      // "-Xlint",
      "-Yno-adapted-args",
      // "-Yno-imports",
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-numeric-widen",
      "-Ywarn-unused-import"
      // "-Ywarn-value-discard"
    ),
    scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits"),
    // scalacOptions in ThisBuild ++= Seq(
    //   "-encoding", "UTF-8",
    //   "-target:jvm-1.6",
    //   "-feature",
    //   "-deprecation",
    //   "-unchecked",
    //   "-language:existentials",
    //   "-language:postfixOps",
    //   "-language:implicitConversions",
    //   "-language:higherKinds",
    //   "-Xlint",
    //   "-Yinline-warnings",
    //   "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
    //   "-Ywarn-inaccessible",
    //   "-Ywarn-dead-code",
    //   "-Xfuture"
    //     // "-Ywarn-unused-import", // noisy, but good to run occasionally
    //     // "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
    //     // "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
    //     //"-Ywarn-numeric-widen", // noisy
    // ),

    javacOptions in (Compile, compile) ++= Seq(
      "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
      "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    ),
    javacOptions in doc ++= Seq("-source", "1.6"),

    javaOptions := Seq(
      "-Xss2m", "-Xms1g", "-Xmx1g", "-Dfile.encoding=UTF8"
    ),

    // 4 x 1GB = 4GB
    concurrentRestrictions in Global := Seq(Tags.limitAll(4))

  )

}
