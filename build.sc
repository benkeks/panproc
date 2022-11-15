import $file.scalablytyped
import mill._, mill.scalalib._, mill.scalajslib._

object main extends ScalaJSModule {
  def scalaVersion = "3.2.1"
  def scalaJSVersion = "1.11.0"
  def moduleDeps = Seq(scalablytyped.`scalablytyped-module`)
}