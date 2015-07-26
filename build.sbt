organization := "io.github"
name := "crawler"

crossScalaVersions := Seq("2.11.7", "2.10.5")
scalaVersion := crossScalaVersions.value.head
scalacOptions := Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= Seq (
    "net.sourceforge.htmlunit" % "htmlunit" % "2.15"
  , "org.specs2" %% "specs2-core" % "3.6.2" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

enablePlugins(GitVersioning)
git.useGitDescribe := true // figure out version from last tag

// required for release to bintray
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
