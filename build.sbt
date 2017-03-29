enablePlugins(ScalaJSPlugin)

name := "NESemul"

scalaVersion := "2.11.8"

libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"

skip in packageJSDependencies := false
jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"

jsDependencies += RuntimeDOM

// uTest settings
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.4.5" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.6"
