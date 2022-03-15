name := "pmml4s"

version := "0.9.15"

organization := "org.pmml4s"

organizationHomepage := Some(new URL("https://pmml4s.org"))

description := "A PMML scoring library in Scala"

homepage := Some(new URL("https://github.com/autodeployai/pmml4s"))

startYear := Some(2017)

licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalacOptions := Seq("-feature", "-language:_", "-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in(Compile, doc) := Seq("-no-link-warnings")

scalaVersion := "2.12.11"

crossScalaVersions := Seq("2.12.11", "2.11.12", "2.13.1", "2.10.7")

libraryDependencies ++= {
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor <= 10 => None
    case _ => Some("org.scala-lang.modules" %% "scala-xml" % "1.2.0")
  }).toSeq ++ Seq(
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "org.apache.commons" % "commons-text" % "1.6",
    "io.spray" %% "spray-json" % "1.3.5",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    "junit" % "junit" % "4.12" % "test"
  )
}

// Exclude src/test/java in the test configuration for scala version less than 2.12
Test / unmanagedSourceDirectories := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor < 12 => (Test / scalaSource).value :: Nil
    case _ => (Test / unmanagedSourceDirectories).value
  }
}

// publishing
updateOptions := updateOptions.value.withGigahorse(false)

publishMavenStyle := true

useGpg := true

// set overwrite to true for snapshot
publishConfiguration := publishConfiguration.value.withOverwrite(isSnapshot.value)
com.typesafe.sbt.pgp.PgpKeys.publishSignedConfiguration := com.typesafe.sbt.pgp.PgpKeys.publishSignedConfiguration.value.withOverwrite(isSnapshot.value)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(isSnapshot.value)
com.typesafe.sbt.pgp.PgpKeys.publishLocalSignedConfiguration := com.typesafe.sbt.pgp.PgpKeys.publishLocalSignedConfiguration.value.withOverwrite(isSnapshot.value)

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra :=
  <scm>
    <url>git://github.com/autodeployai/pmml4s.git</url>
    <connection>scm:git:git@github.com:autodeployai/pmml4s.git</connection>
  </scm>
    <developers>
      <developer>
        <id>scorebot</id>
        <name>Score Bot</name>
      </developer>
    </developers>
