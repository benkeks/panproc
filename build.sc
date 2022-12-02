import mill._, mill.scalalib._

object main extends ScalaModule {
  def scalaVersion = "3.1.3"

  def ivyDeps = Agg(
    ivy"sh.almond:scala-kernel-api-cross-3.1.3_2.13.7:0.13.2"
  )

  // scala-kernel-api depends on com.github.jupyter:jvm-repr:0.4.0 that can be found in...
  def repositoriesTask = T.task { super.repositoriesTask() ++ Seq(
    coursier.maven.MavenRepository("https://maven.scijava.org/content/repositories/public")
  ) }

  def moduleDeps = Seq(
    )
}