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
    assert(pack(l1) == l2)

    val l1a = l1 ::: List('f)
    val l2a = l2 ::: List(List('f))
    assert(pack(l1a) == l2a)

    val l3 = List(1, 2, 3, 4)
    assert(pack(l3) == l3.map { List(_) })
    assert(pack(Nil) == Nil)
    assert(pack(List(1)) == List(List(1)))
    assert(pack(List(1, 1)) == List(List(1, 1)))

  }

  test("P10 Run-length encoding of a list. use pack and map ") {
    val l1 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val l2 = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    assert(encode(l1) == l2)
    assert(encode(Nil) == Nil)

  }

  test("P11 Modified run-length encoding. use pack and map ") {
    val l1 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val l2 = List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
    assert(encodeModified(l1) == l2)
    assert(encodeModified(Nil) == Nil)
  }

  test("P12 Decode a run-length encoded list.") {
    val l1 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val l2 = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    assert(decode(l2) == l1)
    assert(decode(Nil) == Nil)
    assert(decode(List((4, 'a))) == List('a, 'a, 'a, 'a))
  }

  test("P13 Run-length encoding of a list (direct solution).") {
    check { (a: List[Boolean]) =>
      // debug(s"a=${a}")
      val res1 = encode(a)
      val res2 = encodeDirect(a)
      //debug(s"res1=${res1}")
      //debug(s"res2=${res2}")
      res1 == res2
    }
    val l1 = List(1, 1, 1, 1)
    assert(encode(l1) == encodeDirect(l1))
    val l2 = List(1)
    assert(encode(l2) == encodeDirect(l2))
    val l3 = List('a, 'a)
    assert(encode(l3) == encodeDirect(l3))

    val l4 = List('a, 'a, 'b)
    assert(encode(l4) == encodeDirect(l4))
  }

  test("P14 Duplicate the elements of a list.") {
    check { (a: List[Int]) =>
      val res = duplicate(a)
      res == (a.zip(a)).map { case (a, b) => List(a, b) }.flatten

    }
    check { (a: List[AnyVal]) => duplicate(a) == duplicateRec(a, Nil) }
  }

  test("P15 Duplicate the elements of a list a given number of times.") {
    val smallIntGen = Gen.choose(-3, 20)

    check { (a: List[Byte]) =>
      val resgen = smallIntGen.map { n =>
        //debug(s"n=${n},a=${a}")
        val res = duplicateN(n, a)
        //debug(s"res =${res}")
        if (n <= 0) {
          res == a
        } else {
          res.size == a.size * n
          res == a.map { x => (1 to n).map { _ => x }.toList }.flatten
        }
        res == duplicateNRec(n, a, Nil)
      }

      Prop.forAll(resgen) { x => x }
    }

    check { (a: List[Int]) => duplicateNRec(2, a, Nil) == duplicateRec(a, Nil) }

  }

  test("P16 Drop every Nth element from a list.") {
    val smallIntGen = Gen.choose(1, 25)
    check { (a: List[Int]) =>
      val resgen = smallIntGen.map { n =>
        debug(s"n={$n},a=${a}")
        val res1 = drop(n, a)

        val res2 = a.zipWithIndex.filter(x => (x._2 + 1) % n != 0).map { _._1 }
        val res3 = dropRec(n, a)
        debug(s"res1=${res1}")
        debug(s"res2=${res2}")
        debug(s"res3=${res3}")
        res1 == res2
        res1 == res3
      }
      Prop.forAll(resgen) { x => x }
    }
  }
}