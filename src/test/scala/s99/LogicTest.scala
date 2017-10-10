package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

class LogicTest extends FunSuite with Checkers {
  import Logic._

  def f1(a: Boolean, b: Boolean): Boolean = and(a, or(a, b))
  def f2(a: Boolean, b: Boolean): Boolean = a and (a or not(b))

  test("P46 Truth tables for logical expressions.") {
    val res = table2(f1)
    debug(s"res=\n${res}")
  }

  test("P47") {
    val res = table2(f2)
    debug(s"res=\n${res}")
    assert(table2(f1) == table2(f2))
  }

  test("P48") {
    assert(gray(1) == List("0", "1"))
    assert(gray(2) == List("00", "01", "10", "11"))
    val res = gray(4)
    assert(res.size == Math.pow(2, 4))
    val g = Gen.choose(0, 1)
    val g4 = for {
      a <- g
      b <- g
      c <- g
      d <- g
    } yield {
      "" + a + b + c + d
    }
    check {
      Prop.forAll(g4) { s =>
        debug(s"s=${s}")
        res.contains(s)
      }
    }
    assert(res.contains("0100"))

  }
}

class HuffmanTest extends FunSuite with Checkers {
  import Huffman._

//  test("empty tree contains nothing") {
//    val tree = End
//    check { (x: Byte) => contains(tree, x) == false }
//  }

  test("one leaf tree contains one ") {
    check { (x: Char) =>
      val tree = Leaf(x)
      contains(tree, x) && contains(tree, x + 1) == false
    }
  }

  test("Tree((a,b),(c)) contains c ") {
    val tree = Branch(Branch(Leaf('a'), Leaf('b')), Leaf('c'))
    assert(contains(tree, 'c'))
    assert(contains(tree, 'd') == false)
  }

  test("encode simple tree (a,b)") {
    assert(encode(Branch(Leaf('a'), Leaf('b')), 'a') == "0")
    assert(encode(Branch(Leaf('a'), Leaf('b')), 'b') == "1")
  }
  test("simple example"){
    val l = List( ("a",5),("b",4),("c",3),("d",2),("e",1))
    val fre = l.sortBy(_._2).map { p => (Leaf(p._1), p._2) }
    val m = merge(fre)
    debug(s"m=\n${m}")
    debug(codify(m.head._1).mkString)
    assert(encode( m.head._1, "a") == "11" )
  }
  test("merge") {
    val l = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    val fre = l.sortBy(_._2).map { p => (Leaf(p._1), p._2) }
    val m = merge(fre)
    debug(s"m=\n${m}")
    debug(codify(m.head._1).mkString)
    assert(encode( m.head._1, "a") == "0" )
  }

}