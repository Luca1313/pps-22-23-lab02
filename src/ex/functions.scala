package ex

import scala.annotation.targetName

object functions extends App:

  // Enables test print
  val print: Boolean = true

  // Task 2a, svolto da solo
  private object Ex3:
    def positive(x: Int): String = x match
      case n if n >= 0 => "positive"
      case _ => "negative"

    val neg: (String => Boolean) => (String => Boolean) = p => (s => !p(s))

    def neg[A](p: A => Boolean): A => Boolean = !p(_)

  if (print) {
    println(Ex3.positive(3)) // positive
    println(Ex3.positive(-3)) // negative
    println(Ex3.neg[Int](_ > 5)(3)) // true
    println(Ex3.neg[String](_ == "")("Prova")) // true
    println(Ex3.neg(_ == "")("Prova")) // true
  }

  // Task 2b, svolto da solo
  private object Ex4:
    val p1: Double => Double => Double => Boolean = x => y => z => (x <= y) && (y == z)
    val p2: (Double, Double, Double) => Boolean = (x, y, z) => x <= y && y == z
    def p3(x: Double)(y: Double)(z: Double): Boolean = (x <= y) && (y == z)
    def p4(x: Double, y: Double, z: Double): Boolean = (x <= y) && (y == z)

  if (print) {
    println(Ex4.p1(1.0)(2.0)(2.0)) // true
    println(Ex4.p2(1.0, 2.0, 2.0)) // true
    println(Ex4.p3(1.0)(2.0)(2.0)) // true
    println(Ex4.p4(1.0, 2.0, 2.0)) // true
  }

  // Task 2b, svolto da solo
  private object Ex5:
    def compose(f: Int => Int, g: Int => Int): Int => Int = compose[Int, Int, Int](f, g)
    @targetName("generic compose")
    def compose[A, B, C](f: B => C, g: A => B): A => C = p => f(g(p))

  if (print) {
    println(Ex5.compose(_ - 1, _ * 2)(5)) // 9
    println(Ex5.compose[Int, String, Boolean](_ == "", _ + "")(5)) // false
  }

  // Task 3, svolto da solo
  private object Ex6:
    @annotation.tailrec
    def gcd(a: Int, b: Int): Int =
      a match
        case end1 if a == 0 => b
        case end2 if b == 0 => a
        case mag if a > b => gcd(b, a % b)
        case min if a < b => gcd(a, b % a)

  if (print) {
    println(Ex6.gcd(12, 8)) // 4
    println(Ex6.gcd(14, 7)) // 7
  }

  // Task 4, svolto da solo
  private object Ex7:
    enum Shape:
      case Square(ldPoint: (Double, Double), side: Double)
      case Rectangle(ldPoint: (Double, Double), base: Double, height: Double)
      case Circle(centralPoint: (Double, Double), radius: Double)

    def perimeter(s: Shape): Double = s match
      case Shape.Square(_, s) => 4 * s
      case Shape.Rectangle(_, b, h) => 2 * (b + h)
      case Shape.Circle(_, r) => 2 * math.Pi * r

    def contains(shape: Shape, point: (Double, Double)): Boolean =
      def isInsideQuadrilateral(pointToCheck: (Double, Double), quadrilateralPoint: (Double, Double),
                                base: Double, height: Double): Boolean =
        pointToCheck._1 > quadrilateralPoint._1 && pointToCheck._1 < quadrilateralPoint._1 + base
          && pointToCheck._2 > quadrilateralPoint._2 && pointToCheck._2 < quadrilateralPoint._2 + height

      def isInsideCircle(pointToCheck: (Double, Double), circlePoint: (Double, Double), radius: Double): Boolean =
        math.sqrt(math.pow(pointToCheck._1 - circlePoint._1, 2) +
          math.pow(pointToCheck._2 - circlePoint._2, 2)) < radius

      shape match
        case Shape.Square(p, s) => isInsideQuadrilateral(point, p, s, s)
        case Shape.Rectangle(p, b, h) => isInsideQuadrilateral(point, p, b, h)
        case Shape.Circle(cp, r) => isInsideCircle(point, cp, r)

  if (print) {
    println(Ex7.perimeter(Ex7.Shape.Square((0, 0), 5))) // 20
    println(Ex7.perimeter(Ex7.Shape.Rectangle((0, 0), 5, 4))) // 18
    println(Ex7.perimeter(Ex7.Shape.Circle((0, 0), 3))) // 6*pi

    println(Ex7.contains(Ex7.Shape.Square((0, 0), 5), (4, 4))) // true
    println(Ex7.contains(Ex7.Shape.Square((4, 4), 5), (8, 5))) // true
    println(Ex7.contains(Ex7.Shape.Square((4, 5), 3), (2, 6))) // false

    println(Ex7.contains(Ex7.Shape.Rectangle((0, 0), 5, 4), (4, 3))) // true
    println(Ex7.contains(Ex7.Shape.Rectangle((3, 3), 2, 4), (4, 6))) // true
    println(Ex7.contains(Ex7.Shape.Rectangle((5, 6), 1, 2), (5, 6))) // false

    println(Ex7.contains(Ex7.Shape.Circle((0, 0), 3), (2, 2))) // true
    println(Ex7.contains(Ex7.Shape.Circle((2, 2), 4), (-1, 0))) // true
    println(Ex7.contains(Ex7.Shape.Circle((2, 2), 4), (-4, 0))) // false
  }

  // Task 5, svolto da solo
  import u02.Optionals.Option.*
  import u02.Optionals.*
  private object Ex8:

    def filter[A](value: Option[A])(predicate: A => Boolean): Option[A] = value match
      case Some(a) if predicate(a) => Some(a)
      case _ => None()

    def map[A](value: Option[A])(predicate: A => Boolean): Option[Boolean] = value match
      case Some(a) => Some(predicate(a))
      case _ => None()

    def fold[A](opt: Option[A])(value: A)(predicate: A => A): A = opt match
      case Some(a) => predicate(a)
      case None() => value

  if (print) {
    println(Ex8.filter(Some(5))(_ > 2)) // Some(5)
    println(Ex8.filter(Some(5))(_ > 8)) // None()
    println(Ex8.filter(None[Int]())(_ > 2)) // None()

    println(Ex8.map(Some(5))(_ > 2)) // Some(true)
    println(Ex8.map(Some(5))(_ > 8)) // Some(false)
    println(Ex8.map(None[Int]())(_ > 2)) // None()

    println(Ex8.fold(Some(5))(1)(_ + 1)) // 6
    println(Ex8.fold(None[Int]())(1)(_ + 1)) // 1
  }