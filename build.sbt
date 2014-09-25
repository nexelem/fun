name := "functional-workshop"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.4.4" % "test")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

    