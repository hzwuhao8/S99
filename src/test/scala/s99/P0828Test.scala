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
    val res1 = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(res1 == List('a, 'b, 'c, 'a, 'd, 'e))
  }
  test("P08 compress empty List ") {
    val res1 = compress(Nil)
    assert(res1 == Nil)
  }
  test("P08 check") {
    val smallString = Gen.listOfN[Char](10, Gen.alphaChar).map { l =>
      val l1 = l.distinct

      val l2 = l1.zipWithIndex.map {
        case (c, index) =>

          (0 to (scala.util.Random.nextInt(index) + 1)).map(c => c)
      }
      (l1, l2.flatten)
    }

    check { (size: Int) =>
      val smallIntGen = Gen.choose(0, 100)
      val l0gen = smallIntGen.flatMap { max => Gen.listOfN(max, Gen.alphaNumChar) }
      val l1gen = l0gen.map { _.distinct }
      val resgen: Gen[Boolean] = l1gen.map { l1 =>
        val l2 = l1.map { c => (0 to (scala.util.Random.nextInt(10) + 1)).map(_ => c) }
        //println(s"l1@gen=${l1}")
        //println(s"l2@gen=${l2}")

        l1 == compress(l2.flatten)
      }
      Prop.forAll(resgen) { x => x }
    }

  }

}