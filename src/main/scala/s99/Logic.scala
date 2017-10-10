package s99

import scala.language.implicitConversions

object Logic extends Log {

  def not(a: Boolean): Boolean = !a
  def and(a: Boolean, b: Boolean): Boolean = a && b
  def or(a: Boolean, b: Boolean): Boolean = a || b
  def nand(a: Boolean, b: Boolean): Boolean = !(and(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = !(nor(a, b))
  def xor(a: Boolean, b: Boolean): Boolean = !(a == b)
  def equ(a: Boolean, b: Boolean): Boolean = a == b
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)
  implicit def b2l(a: Boolean) = new Logic(a)

  def table2(f: (Boolean, Boolean) => Boolean): String = {
    val fmt = s"%-5s %-5s %-5s\n"
    val head = fmt.format("A", "B", "Result")
    val res = for {
      a <- List(true, false)
      b <- List(true, false)
    } yield {
      fmt.format(a, b, f(a, b))
    }

    head + res.mkString
  }
}
class Logic(a: Boolean) {
  import Logic.not
  import Logic.b2l
  def and(b: Boolean) = a && b

  def or(b: Boolean): Boolean = a || b
  def nand(b: Boolean): Boolean = not(and(b))
  def nor(b: Boolean): Boolean = not(nor(b))
  def xor(b: Boolean): Boolean = not(a == b)
  def equ(b: Boolean): Boolean = a == b
  def impl(b: Boolean): Boolean = not(a) or (b)

}