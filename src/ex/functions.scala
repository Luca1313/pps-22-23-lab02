package ex

object functions extends App:

  def positive(x: Int): String = x match
    case n if n >= 0 => "positive"
    case _ => "negative"

  val neg: (String => Boolean) => (String => Boolean) = p => (s => !p(s))

  def neg[A](p: A => Boolean): A => Boolean = !p(_)

  val empty: String => Boolean = _ == ""

  val notEmpty = neg(empty) // which type of notEmpty?
  notEmpty("foo") // true
  notEmpty("") // false
  notEmpty("foo") && !notEmpty("") // true.. a comprehensive test

  val p1: Double => Double => Double => Boolean = x => y => z => (x <= y) && (y == z)
  val p2: (Double, Double, Double) => Boolean = (x, y, z) => x <= y && y == z
  def p3(x: Double)(y: Double)(z: Double): Boolean = (x <= y) && (y == z)
  def p4(x: Double, y: Double, z: Double): Boolean = (x <= y) && (y == z)

  def compose(f: Int => Int, g: Int => Int): Int => Int = p => f(g(p))
