val scala3Version = "3.3.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "panproc",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    resolvers += "jitpack" at "https://jitpack.io",
    Compile / scalaSource := baseDirectory.value / "main" / "src",
    Test / scalaSource := baseDirectory.value / "test" / "src",
    scalacOptions += "-deprecation",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.github.jupyter" % "jvm-repr" % "0.4.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    ivyXML :=
      <dependencies>
        <dependency org="sh.almond" name="scala-kernel-api-cross-3.2.2_2.13.10" rev="0.13.14"/>
      </dependencies>,
  )
