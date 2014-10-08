name := "crawler"

version := "0.6.0"

scalaVersion := "2.10.2"

scalaBinaryVersion := "2.10"

scalacOptions := Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= Seq (
    "net.sourceforge.htmlunit" % "htmlunit" % "2.15"
  , "org.specs2" %% "specs2" % "1.14" % "test"
)
