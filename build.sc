import mill._, mill.scalalib._

object main extends ScalaModule {
  def scalaVersion = "3.3.1"

  def scalacOptions = Seq("-deprecation")

  def ivyDeps = Agg(
    ivy"sh.almond:scala-kernel-api-cross-3.3.1_2.13.11:0.14.0-RC14"
  )

  // scala-kernel-api depends on com.github.jupyter:jvm-repr:0.4.0 that can be found in...
  def repositoriesTask = T.task { super.repositoriesTask() ++ Seq(
    coursier.maven.MavenRepository("https://maven.scijava.org/content/repositories/public")
  ) }

  def moduleDeps = Seq(
    )
}