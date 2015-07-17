name := "crawler"

version := "0.6.0"

scalacOptions := Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= Seq (
    "net.sourceforge.htmlunit" % "htmlunit" % "2.15"
  , "org.specs2" %% "specs2-core" % "3.6.2" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

