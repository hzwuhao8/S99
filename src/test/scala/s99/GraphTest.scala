package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

class GraphTest extends FunSuite with Checkers with Log {

  test("") {
    val g1 = Graph.term2(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
      List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h')))

    debug(s"g1=${g1}")
    val g2 = Graph.adjacent2(List(('b', List('c', 'f')), ('c', List('b', 'f')), ('d', Nil),
      ('f', List('b', 'c', 'k')), ('g', List('h')), ('h', List('g')),
      ('k', List('f'))))

    debug(s"g2=${g2}")

    assert(g1 == g2)

    val t1 = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm

    debug(s"t1=${t1}")
  }
}