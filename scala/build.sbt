organization := "com.ubiregi"

name := "Ubiregi_API_Client"

version := "0.1.0"

scalaVersion := "2.9.1"

seq(assemblySettings: _*)

resolvers ++= Seq(
  "jboss repo" at "http://repository.jboss.org/nexus/content/groups/public-jboss/"
)

libraryDependencies ++= Seq(
   "net.databinder" %% "dispatch-http" % "0.8.7",
   "net.databinder" %% "dispatch-json" % "0.8.7"
)
