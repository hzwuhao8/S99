package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

class GraphTest extends FunSuite with Checkers with Log {

  test("P80") {
    val g1 = Graph.term(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
      List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h')))

    debug(s"g1=${g1}")
    val g2 = Graph.adjacent2(List(('b', List('c', 'f')), ('c', List('b', 'f')), ('d', Nil),
      ('f', List('b', 'c', 'k')), ('g', List('h')), ('h', List('g')),
      ('k', List('f'))))

    debug(s"g2=${g2}")

    assert(g1 == g2)

    val t1 = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm

    debug(s"t1=${t1}")

    val t2 = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]")
    debug(s"t2.toTermForm=${t2.toTermForm}")
    debug(s"t2.toAdjacentForm=${t2.toAdjacentForm}")

  }

  test("P81") {
    val g = Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]")
    val path = g.findPaths("p", "q")
    debug(s"path=${path}")

    val path2 = g.findPaths("p", "k")
    debug(s"path2=${path2}")

    assert(path2 == Nil)
  }

  test("82") {
    val g = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]")
    debug(s"g=${g.toTermForm()}")
    debug(s"path k -> f =  ${g.findPaths("k", "f")}")
    debug(s"${g.nodes.get("b").map { _.neighbors }}")
    debug(s"${g.nodes.get("c").map { _.neighbors }}")
    debug(s"path b -> f =  ${g.findPaths("b", "f")}")
    val p = g.findCycles("f")
    debug(s"p=${p}")
  }

  test("P83") {
    {
      val g = Graph.fromString("[a-b, b-c, a-c]")
      val trees = g.spanningTrees
      debug(s"trees=\n${trees.map(_.toTermForm()).distinct.mkString("\n")}")

    }
    {
      //http://mathworld.wolfram.com/AndrasfaiGraph.html
      val g = Graph.fromString("[a-b, b-c, c-d,d-e,e-a]")
      val trees = g.spanningTrees
      val termFormatList = trees.map(_.toTermForm()).distinct
      debug(s"trees=\n${termFormatList.mkString("\n")}")
      assert(termFormatList.size == 5)
    }
  }

  test("P84") {
    {
      val g = Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]")
      val tree = g.minimalSpanningTree

      debug(s"trees=\n${tree.toTermForm()}")
    }
    {
      val g = Graph.termLabel(
        List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
        List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
          ('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
          ('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1)))

      val tree = g.minimalSpanningTree

      debug(s"trees=\n${tree.toTermForm()}")
    }
  }
  
  test("P85"){
    val g1 = Graph.fromString("[a-b]")
    val g2 = Graph.fromString("[5-7]")
    debug(s"g1=${g1.toTermForm()}")
    debug(s"g2=${g2.toTermForm()}")
    debug(s"g1 <=> g1 = ${g1.isIsomorphicTo(g1)}")
    debug(s"g1 <=> g2 = ${g1.isIsomorphicTo(g2)}")
  }
  
  test("P86"){
    val g = Graph.fromString("[a-b, b-c, a-c, a-d]")
    debug(s"res=${g.nodesByDegree()}")
    debug(s"colorNodes=${g.colorNodes()}")
  }
  test("P87"){
    
  }
}