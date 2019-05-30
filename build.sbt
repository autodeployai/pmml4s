name := "pmml4s"

version := "0.9.0"

organization := "org.pmml4s"

organizationHomepage := Some(new URL("http://pmml4s.org"))

description := "A PMML (Predictive Model Markup Language) scoring library in Scala"

homepage := Some(new URL("https://github.com/autodeployai/pmml4s"))

startYear := Some(2017)

licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalacOptions := Seq("-feature", "-language:_", "-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in(Compile, doc) := Seq("-no-link-warnings")

libraryDependencies ++= {
  Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.1.0",
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "io.spray" %% "spray-json" % "1.3.5",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
}

crossScalaVersions := Seq("2.12.8", "2.11.12")
