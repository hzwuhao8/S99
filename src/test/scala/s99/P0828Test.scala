package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

/**
 * P08  - P28
 *
 */
class P0828Test extends FunSuite with Checkers {
  import P0828._
  test("P08 Eliminate consecutive duplicates of list elements.") {
    val l1 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val l2 = List('a, 'b, 'c, 'a, 'd, 'e)
    val res1 = compress(l1)
    assert(res1 == l2)
    val res2 = compress2(l1)
    assert(res2 == l2)

  }
  test("P08 compress empty List ") {
    val res1 = compress(Nil)
    assert(res1 == Nil)
    val res2 = compress2(Nil)
    assert(res2 == Nil)
  }
  test("P08 check") {
    val smallIntGen = Gen.choose(0, 100)
    val l0gen = smallIntGen.flatMap { max => Gen.listOfN(max, Gen.alphaNumChar) }
    val l1gen = l0gen.map { _.distinct }
    val resgen: Gen[Boolean] = l1gen.map { l1 =>
      val l2 = l1.map { c => (0 to (scala.util.Random.nextInt(10) + 1)).map(_ => c) }
      //println(s"l1@gen=${l1}")
      //println(s"l2@gen=${l2}")
      val l3 = l2.flatten
      l1 == compress(l3) && l1 == compress2(l3)

    }

    check {
      Prop.forAll(resgen) { x => x }
    }

  }
  test("P08  compress == compress2 ") {
    check { (a: List[AnyVal]) => compress(a) == compress2(a) }
  }

  test("P09  Pack consecutive duplicates of list elements into sublists.") {
    val l1 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val l2 = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    val res1 = pack(l1)
    assert(res1 == l2)
    val l3 = List(1, 2, 3, 4)
    assert(pack(l3) == l3.map { List(_) })
    assert(pack(Nil) == Nil)
    assert(pack(List(1)) == List(List(1)))
    assert(pack(List(1,1)) == List(List(1,1)))
    
  }
}