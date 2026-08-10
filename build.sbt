name := "ProbGen"
scalaVersion := "3.7.4"

enablePlugins(ScalaJSPlugin)

Compile / scalaSource := file("src")

// ScalaJS settings
scalaJSUseMainModuleInitializer := true
mainClass := Some("info.kwarc.probgen.Main")

// NoModule = single plain .js file, easiest for opening index.html directly
scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) }

// DOM access from Scala
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"
