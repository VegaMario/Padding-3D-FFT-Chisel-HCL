ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
      scalaVersion := "2.13.7",
      addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.0" cross CrossVersion.full),
      libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.5.0",
      // We also recommend using chiseltest for writing unit tests
      libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.0" % "test",
      libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0",
    name := "Padding-3D-FFT-Chisel-HCL"
  )
