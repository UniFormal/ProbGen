name := "ProbGen"
scalaVersion := "3.7.4"

enablePlugins(ScalaJSPlugin)

// absolute, so tooling (bloop/Metals) records a source *directory* rather than a
// frozen list of files -- with a relative path, newly added files stay invisible
// to the IDE until the build is re-imported
Compile / scalaSource := baseDirectory.value / "src"

// ScalaJS settings
scalaJSUseMainModuleInitializer := true
//mainClass := Some("info.kwarc.probgen.main")
Compile / mainClass := Some("info.kwarc.probgen.main")
//Compile / discoveredMainClasses := Seq("info.kwarc.probgen.main")

// NoModule = single plain .js file, easiest for opening index.html directly
scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) }

// DOM access from Scala
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"

// Testing
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.20"
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.20" % "test"
