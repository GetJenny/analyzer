import NativePackagerHelper._

name := "analyzer"

organization := "com.getjenny"

crossScalaVersions := Seq("2.12.8", "2.11.11")

resolvers ++= Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.bintrayRepo("hseeberger", "maven"))

libraryDependencies ++= {
  val BreezeVersion	= "0.13.2"
  val ScalatestVersion	= "3.0.5"
  val ScalazVersion	= "7.2.24"
  val ScoptVersion	= "3.7.0"
  Seq(
    "com.github.scopt" %% "scopt" % ScoptVersion,
    "org.scalanlp" %% "breeze" % BreezeVersion,
    "org.scalanlp" %% "breeze-natives" % BreezeVersion,
    "org.scalatest" %% "scalatest" % ScalatestVersion % Test,
    "org.scalaz" %% "scalaz-core" % ScalazVersion
  )
}

scalacOptions += "-deprecation"
scalacOptions += "-feature"
//scalacOptions += "-Ylog-classpath"
testOptions in Test += Tests.Argument("-oF")

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)
enablePlugins(UniversalPlugin)

git.useGitDescribe := true

fork in Test := true

// do not buffer test output
logBuffered in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

releaseCrossBuild := true

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

homepage := Some(url("http://www.getjenny.com"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/GetJenny/analyzer"),
    "scm:git@github.com:GetJenny/analyzer.git"
  )
)

developers := List(
  Developer(
    id    = "angleto",
    name  = "Angelo Leto",
    email = "angelo@getjenny.com",
    url   = url("http://www.getjenny.com")
  )
)

releaseProcess := Seq[ReleaseStep](
		releaseStepCommand("sonatypeOpen \"com.getjenny\" \"analyzer\""),
		releaseStepCommand("publishSigned"),
		releaseStepCommand("sonatypeRelease")
)

licenses := Seq(("GPLv2", url("https://www.gnu.org/licenses/old-licenses/gpl-2.0.md")))

