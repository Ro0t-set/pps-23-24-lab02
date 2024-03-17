package task02

import u02.AnonymousFunctions.l

object Main extends App:

// 3a)
// Using match-cases, implement the following function from Int to
// String: positive(x) =
// (“positive” if x ≥ 0 “negative” if x < 0
// in both of the following styles: (i) val assigned to function literal
// (lambda) and (ii) method syntax.

  def getTheSignMethodStyle(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _           => "negative"

  val getTheSignLambdaStyle: Int => String = _ match
    case x if x >= 0 => "positive"
    case _           => "negative"

// 3b)
// Implement a neg function that accepts a predicate on strings (i.e., a
// function from strings to Booleans) and returns another predicate on strings, namely, one that does the exact opposite;
// write the type first, and then define the function both as a val lambda and with method syntax val empty:
// String => Boolean = _ == "" // predicate on strings
// val notEmpty = neg(empty) // which type of notEmpty?
// notEmpty("foo") // true
// notEmpty("") // false
// notEmpty("foo") && !notEmpty("") // true.. a comprehensive test

  val emptyString: String => Boolean = _ == ""

  println(emptyString(""))

  def negMethodStyle(s: String => Boolean): String => Boolean = !s(_)

  val negLambdaStyle: (String => Boolean) => String => Boolean = s => !s(_)

//3c)
//Make neg work for generic predicates, and write tests to check it (therefore, neg will be generic: def neg[X]...).

  def negMethodStyleGeneric[A](s: A => Boolean): A => Boolean = !s(_)

//4) Currying
// Implement a predicate that checks whether its arguments x,y,z respect
// the relation x ≤ y = z, in 4 variants (curried/non-curried × val/def)

  def xyzStandard(x: Double, y: Double, z: Double): Boolean = x <= y && y == z

  def xyzCurryed(x: Double)(y: Double)(z: Double): Boolean = x <= y && y == z

  val xyzLambdaStandard: (Double, Double, Double) => Boolean = (x, y, z) =>
    x <= y && y == z

  val xyzLambdaCurryed: Double => Double => Double => Boolean = x =>
    y => z => x <= y && y == z

//5)
//Create a function that implements functional compositions (f ◦ g)(x) = f (g(x))
// ▶ Signature: compose(f: Int => Int, g: Int => Int): Int => Int ▶ Example: compose(_ - 1, _ * 2)(5) // 9
// ▶ Create a generic version of compose
// What signature? Is there any constraint?

  def compose(f: Int => Int, g: Int => Int): Int => Int = i => f(g(i))

  def genericCompose[A, B, C](f: B => C, g: A => B): A => C = i => f(g(i))

//6. Create a function to compute the greatest common divisor (GCD) of two integers a and b
// ▶ The GCD is the largest positive integer that divides both a and b without leaving a remainder.
// ▶ Signature: gcd(a: Int, b: Int): Int
// ▶ Example: (gcd(12, 8), gcd(14, 7)) // (4, 7)
// ▶ Hint: Use the Euclidean algorithm, which states that if a and b are two
// integers with a > b, then gcd(a, b) = gcd(b, a mod b).
// ▶ Hint: For mod, use the same operator you would use in Java
// ▶ Write a tail-recursive version of the function using the same approach
// as the tail-recursive Factorial function seen in lecture 02.

  @annotation.tailrec
  def gcd(a: Int, b: Int): Int = b match
    case 0 => a
    case _ => gcd(b, a % b)

// 7. Define a set of geometric shapes and methods for calculating their perimeter and area
// ▶ Define an enum Shape
// ▶ Define concrete types Rectangle, Circle, and Square; these product
// types should exhibit the typical geometric properties you would expect
// to characterise the corresponding shapes
// ▶ Define a module with two methods perimeter(shape: Shape): Double
// and scale(shape: Shape, alpha: Double): Shape for computing perimeter and scaling a shape, respectively
// scaling means multiplying the dimensions of the shape by a factor α
// ▶ You may want to address this exercise through a TDD process

  enum Shape:
    case Rectangle(w: Double, h: Double)
    case Circle(r: Double)
    case Square(l: Double)

  object Shape:

    def perimeter(shape: Shape): Double = shape match
      case Rectangle(w, h) => (w + h) * 2
      case Circle(r)       => 2 * Math.PI * r
      case Square(l)       => l * 4

    def scale(shape: Shape, alpha: Double): Shape = shape match
      case Rectangle(w, h) => Rectangle(w * alpha, h * alpha)
      case Circle(r)       => Circle(r * alpha)
      case Square(l)       => Square(l * alpha)

// 8. Look at tasks5.Optionals:
// ▶ This follows the concept of Java Optional but with an ADT
// approach, therefore describing the Optional with two cases: Maybe[A](value: A): the value is present
// Empty() : the value is not present
// ▶ Look at the implementation and the tests
// ▶ Implement map: a function that transform the value (if present)– for
// more details look at the tests
//         map(Maybe(5))(_ > 2) // Maybe(true)
// map(Empty())(_ > 2) // Empty
// ▶ filter: a function that keeps the value (if present, otherwise the output
// is None) only if it satisfies the given predicate. filter(Maybe(5))(_ > 2) // Maybe(5) filter(Maybe(5))(_ > 8) // Empty filter(Empty())(_ > 2) // Empty
// The signature can be straightforwardly guessed by the examples.

  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:

    def isEmpty[A](o: Optional[A]): Boolean = o match
      case Empty() => true
      case _       => false

    def orElse[A, B >: A](o: Optional[A], default: B): B = o match
      case Maybe(v) => v
      case _        => default

    def map[A, B](o: Optional[A])(f: A => B): Optional[B] = o match
      case Maybe(v) => Maybe(f(v))
      case _        => Empty()

    def filter[A](o: Optional[A])(f: A => Boolean): Optional[A] = o match
      case Maybe(v) if (f(v)) => Maybe(v)
      case _                  => Empty()
