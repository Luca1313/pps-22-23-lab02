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

  //def compose[A, B, C](f: B => C, g: A => B): A => C = p => f(g(p))

  @annotation.tailrec
  def gcd(a: Int, b: Int): Int =
    a match
      case end1 if a == 0 => b
      case end2 if b == 0 => a
      case mag if a > b => gcd(b, a % b)
      case min if a < b => gcd(a, b % a)

  enum Shape:
    case Square(ldPoint: (Double, Double), side: Double)
    case Rectangle(ldPoint: (Double, Double), base: Double, height: Double)
    case Circle(centralPoint: (Double, Double), radius: Double)

  def perimeter(s: Shape): Double = s match
    case Shape.Square(_, s) => 4*s
    case Shape.Rectangle(_, b, h) => 2*(b+h)
    case Shape.Circle(_, r) => 2*math.Pi*r

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