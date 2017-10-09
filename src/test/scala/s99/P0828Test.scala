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
  test("P17 Split a list into two parts.") {
    check { (n: Int, a: List[Int]) =>
      split(n, a) == a.splitAt(n)
    }
    check { (n: Int, a: List[Int]) =>
      splitRec(n, a, (Nil, Nil)) == a.splitAt(n)
    }
  }

  test("P18 Extract a slice from a list.") {
    check { (from: Byte, until: Byte, a: List[Int]) =>
      val res1 = slice(from, until, a)
      val res2 = a.slice(from, until)
      val res3 = sliceRec(from, until, a, Nil)
      debug(s"res1=${res1}")
      debug(s"res2=${res2}")
      debug(s"res3=${res3}")
      res1 == res2
      res3 == res2
    }
  }

  test("P19 Rotate a list N places to the left.") {
    val l1 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    val l2 = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    val res1 = rotate(3, l1)
    assert(res1 == l2)

    val l3 = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    val res2 = rotate(-2, l1)
    assert(res2 == l3)
  }

  test("P20 Remove the Kth element from a list.") {
    val l1 = List('a, 'b, 'c, 'd)
    val l2 = (List('a, 'c, 'd), 'b)
    val res = removeAt(1, List('a, 'b, 'c, 'd))
    assert(res == l2)

  }

  test("P21 Insert an element at a given position into a list.") {
    val l1 = List('a, 'b, 'c, 'd)
    val l2 = List('a, 'new, 'b, 'c, 'd)
    val a = 'new
    val res = insertAt(a, 1, l1)
    assert(res == l2)
    assert(insertAt(a, -1, l1) == a :: l1)
    assert(insertAt(a, 0, l1) == a :: l1)
    assert(insertAt(a, l1.size, l1) == l1 ::: List(a))
  }

  test("P22 Create a list containing all integers within a given range.") {
    val g1 = Gen.choose(0, 10)
    val g2 = Gen.choose(10, 20)
    val g3 = for {
      s <- g1
      t <- g2
    } yield {
      val res1 = range(s, t)
      val res2 = s.to(t).toList
      debug(s"s=${s}\tt=${t}\tres1=${res1}")
      debug(s"s=${s}\tt=${t}\tres1=${res2}")
      (res1, res2)
    }
    check { Prop.forAll(g3) { p => p._1 == p._2 } }
  }

  test("P23 randomSelect") {
    check { (n: Int, xs: List[Int]) =>
      if (n >= 0 && n <= xs.size) {
        randomSelect(n, xs).size == n
      } else if (n > 0 && n >= xs.size) {
        randomSelect(n, xs).size == xs.size
      } else {
        randomSelect(n, xs).size == 0
      }
    }
  }
  test("P24 Lotto: Draw N different random numbers from the set 1..M.") {
    val gen1 = Gen.choose(1, 8)
    val gen2 = Gen.choose(16, 48)
    val gen3 = for {
      n <- gen1
      m <- gen2
    } yield {
      val res = lotto(n, m)
      (n, m, res)
    }

    check {
      Prop.forAll(gen3) {
        case (n, m, res) =>
          res.size == n && res.toSet.size == n && res.forall(x => x <= m && x >= 1)
      }
    }
  }

  test("P25 Generate a random permutation of the elements of a list.") {
    check { (a: List[Int]) =>
      val res1 = randomPermute(a)
      res1.size == a.size
    }
  }
  test("P26 Generate the combinations of K distinct objects chosen from the N elements of a list.") {
    def c(n: Int, i: Int): Int = {
      require(n > 0 && i > 0 && n > i)
      val l1 = n.to((n - i + 1), -1)
      val l2 = 1.to(i)
      l1.product / l2.product
    }

    val l1 = List('a, 'b, 'c, 'd, 'e, 'f)

    val res0 = combinations(0, l1)
    assert(res0 == Nil)
    val res1 = combinations(1, l1)
    assert(res1 == l1.map { List(_) })
    val res2 = combinations(2, l1)
    assert(res2.size == c(l1.size, 2))
    val res3 = combinations(3, l1)
    assert(res3.size == c(l1.size, 3))

    val res4 = combinations(4, l1)
    assert(res4.size == c(l1.size, 4))

    val res5 = combinations(5, l1)
    debug(s"res5=${res5}")
    assert(res5.size == c(l1.size, 5))
    //pending
  }

  test("P27 Group the elements of a set into disjoint subsets.") {
    assert(group(Nil, Nil) == Nil)
    assert(group(List(0), Nil) == Nil)
    check { (as: Set[Int]) =>
      val bs = as.toList
      if (bs.isEmpty) {
        group(List(bs.size), bs) == Nil
      } else {
        group(List(bs.size), bs) == List(List(bs))
      }
    }

    val res1 = group(List(1, 2), List('a, 'b, 'c))
    debug(s"res1=${res1.mkString("\n", "\n", "\n")}")
    assert(res1.contains(List(List('a), List('b, 'c))))

    val res2 = group(List(1, 1, 1), List('a, 'b, 'c))
    debug(s"res2=${res2.mkString("\n", "\n", "\n")}")
    assert(res2.contains(List(List('a), List('b), List('c))))

    val l1 = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    val l2 = List(2, 2, 5)
    val res3 = group(l2, l1)

    assert(res3.contains( List( List("Aldo", "Beat" ),List("Carla", "David" ),List( "Evi", "Flip", "Gary", "Hugo", "Ida")  ) ))
    //pending
  }

  test("P28 Sorting a list of lists according to length of sublists.") {
    val l1 = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val res = lsort(l1)
    val l2 = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
    assert(res == l2)

  }
  test("P28 lsortFreq") {
    val l1 = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
    val res = lsortFreq(l1)
    val l2 = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
    assert(res == l2)
  }
}