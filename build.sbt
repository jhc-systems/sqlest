import com.typesafe.sbt.pgp.PgpKeys
import com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo
import com.typesafe.sbt.SbtGhPages
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import com.typesafe.sbt.SbtSite
import ReleaseTransformations._
import scalariform.formatter.preferences._

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(sqlest, extractors, examples)

lazy val sqlest = (project in file("sqlest"))
  .settings(moduleName := "sqlest")
  .settings(sqlestSettings: _*)
  .settings(
    tutSourceDirectory := file("docs") / "sqlest",
    tutTargetDirectory := file("."),
    libraryDependencies ++= Seq("com.typesafe.scala-logging" %% "scala-logging" % "3.1.0")
  ).dependsOn(extractors)

lazy val extractors = (project in file("extractors"))
  .settings(moduleName := "sqlest-extractors")
  .settings(sqlestSettings: _*)
  .settings(
    tutSourceDirectory := file("docs") / "extractors",
    tutTargetDirectory := file("extractors"),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "joda-time" % "joda-time" % "2.3",
      "org.joda" % "joda-convert" % "1.6"
    )
  )

lazy val examples = (project in file("examples"))
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(libraryDependencies += "com.h2database" % "h2" % "1.4.180")
  .dependsOn(sqlest)

lazy val commonSettings = SbtScalariform.scalariformSettings ++ publishingSettings ++ Seq(
  organization := "uk.co.jhc",
  scalaVersion := "2.11.6",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Yinline-warnings",
    "-Ywarn-dead-code",
    "-Xfuture"
  ),
  coverageExcludedPackages := "sqlest.examples",
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(DanglingCloseParenthesis, Preserve)
)

lazy val sqlestSettings = commonSettings ++ scaladocSettings ++ tutSettings ++ Boilerplate.settings ++ Seq(
  tutNameFilter := """README.md""".r,
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "com.chuusai" %% "shapeless" % "2.1.0" % "test",
    "com.h2database" % "h2" % "1.4.180" % "test"
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val scaladocSettings = SbtSite.site.settings ++ SbtSite.site.includeScaladoc() ++ SbtGhPages.ghpages.settings ++ Seq(
  gitRemoteRepo := "git@github.com:jhc-systems/sqlest.git"
)

lazy val publishingSettings = sonatypeSettings ++ sonatypeReleaseProcess ++ Seq(
  // Publishing - http://www.scala-sbt.org/0.13/docs/Using-Sonatype.html
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  credentials := {
    Seq("SONATYPE_USER", "SONATYPE_PASSWORD").map(sys.env.get) match {
      case Seq(Some(user), Some(password)) =>
        Seq(Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, password))
      case _ =>
        credentials.value
    }
  },
  pomIncludeRepository := { _ => false },
  pomExtra := (
    <url>https://github.com/jhc-systems/sqlest</url>
    <licenses>
      <license>
        <name>Apache License, Version 2.0</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com/jhc-systems/sqlest.git</url>
      <connection>scm:git:git@github.com/jhc-systems/sqlest.git</connection>
    </scm>
    <developers>
      <developer>
        <id>davegurnell</id>
        <name>Dave Gurnell</name>
      </developer>
      <developer>
        <id>brendanator</id>
        <name>Brendan Maginnis</name>
      </developer>
    </developers>)
)

lazy val sonatypeReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _)),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeRelease", _)),
    pushChanges
  )
)
