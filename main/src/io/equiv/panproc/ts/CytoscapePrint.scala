package io.equiv.panproc.ts

import jupyter.{Displayer, Displayers}
import scala.collection.JavaConverters.*

object CytoscapePrint:

  var cytoscapeUrl = "../node_modules/cytoscape/dist/cytoscape.esm.min.js"

  def setup(): Unit =
    Displayers.register(
      classOf[TransitionSystem[?, ?, ?]],
      new Displayer[TransitionSystem[?, ?, ?]]:
        override def display(ts: TransitionSystem[?, ?, ?]): java.util.Map[String, String] =
          val nodeStrings =
            for
              (id, label) <- ts.nodeLabeling
            yield s"{ data: { id: '$id', name: '$label'} }"
          val edgeStrings =
            for
              (src, label, target) <- ts.step.tupleSet
            yield s"{ data: { source: '$src', target: '$target', label: '$label'} }"
          Map("text/html" ->
            buildCytoscape(
              nodeStrings.mkString("[", ",", "]"),
              edgeStrings.mkString("[", ",", "]"),
              height = 100 + 150 * Math.pow(ts.nodeLabeling.size, .5).toInt
            )).asJava
    )

  def buildCytoscape(nodesString: String, edgesString: String, height: Int = 300) =
    s"""<div id="cy" style="width: 100%; height: ${height}px;"></div>
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
