name := "pmml4s"

organization := "org.pmml4s"

organizationHomepage := Some(new URL("https://pmml4s.org"))

description := "A PMML scoring library in Scala"

homepage := Some(new URL("https://github.com/autodeployai/pmml4s"))

startYear := Some(2017)

licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalacOptions := Seq(
  "-feature",
  "-language:_",
  "-unchecked",
  "-deprecation",
  "-encoding",
  "utf8",
  "-language:implicitConversions",
) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, scalaMajor)) if scalaMajor <= 11 => Seq.empty
  case _ => Seq(
    "-optimize",
    "-opt:box-unbox",
    "-opt:l:method",
    "-opt:l:inline",
    "-opt-inline-from:**"
  )
})

scalacOptions in(Compile, doc) := Seq("-no-link-warnings")

scalaVersion := "2.13.14"

crossScalaVersions := Seq("2.13.14", "2.11.12", "2.12.18", "3.1.3")

libraryDependencies ++= {
  Seq(
    "org.apache.commons" % "commons-lang3" % "3.15.0",
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "org.apache.commons" % "commons-text" % "1.12.0",
    "io.spray" %% "spray-json" % "1.3.6",
    "org.scalatest" %% "scalatest" % "3.2.15" % "test",
    "junit" % "junit" % "4.13.2" % "test"
  )
}

// Exclude src/test/java in the test configuration for scala version less than 2.12
Test / unmanagedSourceDirectories := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor < 12 => (Test / scalaSource).value :: Nil
    case _ => (Test / unmanagedSourceDirectories).value
  }
}

ThisBuild / developers := List(
    Developer(
      "scorebot",
      "Score Bot",
      "scorebot@outlook.com",
      url("https://github.com/scorebot")
    )
  )
