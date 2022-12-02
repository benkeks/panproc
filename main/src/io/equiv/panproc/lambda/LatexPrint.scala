package io.equiv.panproc.lambda

import jupyter.{Displayer, Displayers}
import scala.collection.JavaConverters._

object LatexPrint:

  def setup(): Unit =
    Displayers.register(classOf[Syntax.Expression], new Displayer[Syntax.Expression] {
      override def display(expr: Syntax.Expression): java.util.Map[String, String] =
        Map("text/plain" -> expr.pretty).asJava
    })