name := "crawler"

version := "0.3.3"

scalaVersion := "2.8.1"

libraryDependencies ++= Seq (
    "net.sourceforge.htmlunit" % "htmlunit" % "2.9"
  , "org.scala-tools.testing" %% "specs" % "1.6.6" % "test"
)
