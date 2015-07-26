resolvers += Resolver.url("2m-sbt-plugins", url("https://dl.bintray.com/2m/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0-7-g913381f")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.4")
