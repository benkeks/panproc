package io.equiv.panproc.ts

import jupyter.{Displayer, Displayers}
import scala.collection.JavaConverters._

object CytoscapePrint:

  var cytoscapeUrl = "../node_modules/cytoscape/dist/cytoscape.esm.min.js"

  def setup(): Unit =
    Displayers.register(classOf[TransitionSystem[_,_,_]], new Displayer[TransitionSystem[_,_,_]] {
      override def display(ts: TransitionSystem[_,_,_]): java.util.Map[String, String] =
        val nodeStrings = for {
          (id, label) <- ts.nodeLabeling.toIterable
        } yield s"{ data: { id: '$id', name: '$label'} }"
        val edgeStrings = for {
          (src, label, target) <- ts.step.tupleSet
        } yield s"{ data: { source: '$src', target: '$target', label: '$label'} }"
        Map("text/html" ->
          buildCytoscape(nodeStrings.mkString("[", ",", "]"), edgeStrings.mkString("[", ",", "]"))
        ).asJava
    })

  def buildCytoscape(nodesString: String, edgesString: String) =
    s"""<div id="cy" style="width: 100%; height: 300px;"></div>
       |
       |<script type="module">
       |import cytoscape from "$cytoscapeUrl";
       |
       |var cy = cytoscape({
       |  container: document.getElementById('cy'),
       |
       |  layout: {
       |    name: 'cose',
       |    padding: 10
       |  },
       |
       |  style: cytoscape.stylesheet()
       |    .selector('node')
       |      .style({
       |        'content': 'data(name)',
       |        'text-valign': 'center'
       |      })
       |    .selector(':selected')
       |      .style({
       |        'border-width': 3,
       |        'border-color': '#333'
       |      })
       |    .selector('edge')
       |      .style({
       |        'opacity': 0.666,
       |        'curve-style': 'bezier',
       |        'target-arrow-shape': 'triangle',
       |        'label': 'data(label)'
       |      }),
       |
       |  elements: {
       |    nodes: $nodesString,
       |    edges: $edgesString
       |  }
       |});
       |</script>""".stripMargin