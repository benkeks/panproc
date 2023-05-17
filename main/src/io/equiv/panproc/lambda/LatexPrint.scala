package io.equiv.panproc.lambda

import jupyter.{Displayer, Displayers}
import scala.jdk.CollectionConverters.MapHasAsJava

object LatexPrint:

  def setup(): Unit =
    Displayers.register(classOf[Syntax.Expression], new Displayer[Syntax.Expression] {
      override def display(expr: Syntax.Expression): java.util.Map[String, String] =
        Map("text/plain" -> expr.pretty).asJava
    })