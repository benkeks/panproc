
package ammonite
package $file
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}
import _root_.ammonite.repl.ReplBridge.value.{
  codeColorsImplicit
}


object build{
/*<script>*/import $file.$            
import mill._, mill.scalalib._

object main extends ScalaModule {
  def scalaVersion = "3.1.3"

  def ivyDeps = Agg(
    ivy"com.github.jupyter:jvm-repr:0.4.0",
    ivy"sh.almond:scala-kernel-api-cross-3.1.3_2.13.7:0.13.2"
  )

  def moduleDeps = Seq(
    )
}/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "build"
  /*</generated>*/
}
