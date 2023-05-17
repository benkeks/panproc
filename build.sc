import mill._, mill.scalalib._

object main extends ScalaModule {
  def scalaVersion = "3.2.2"

  def scalacOptions = Seq("-deprecation")

  def ivyDeps = Agg(
    ivy"sh.almond:scala-kernel-api-cross-3.2.2_2.13.10:0.13.14"
  )

  // scala-kernel-api depends on com.github.jupyter:jvm-repr:0.4.0 that can be found in...
  def repositoriesTask = T.task { super.repositoriesTask() ++ Seq(
    coursier.maven.MavenRepository("https://maven.scijava.org/content/repositories/public")
  ) }

  def moduleDeps = Seq(
    )
}