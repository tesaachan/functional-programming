import java.util.concurrent.TimeUnit
// Type-directed programming and type-classes

trait Ordering[A]:
    def compare(a1: A, a2: A): Int

object Ordering:
    implicit val Int: Ordering[Int] =
        new Ordering[Int]:
            def compare(x: Int, y: Int) = if (x < y) -1 else if (x > y) 1 else 0

  implicit val String: Ordering[String] =
      new Ordering[String]:
          def compare(s: String, t: String) = s.compareTo(t)

def sort[A: Ordering](xs: List[A]): List[A] = ???

// In these terms 'Ordering' is a type-class, instances of 'Ordering' are types.

trait Ring[A]:
    def plus(x: A, y: A): A
    def mult(x: A, y: A): A
    def inverse(x: A): A
    def zero: A
    def one: A

object Ring:
    implicit val ringInt: Ring[Int] = new Ring[Int]:
        def plus(x: Int, y: Int): Int = x + y
        def mult(x: Int, y: Int): Int = x * y
        def inverse(x: Int): Int = -x
        def zero: Int = 0
        def one: Int = 1

// define a function that checks that the + associativity law is satisfied by a given Ring instance:
def plusAssociativity[A](x: A, y: A, z: A)(implicit ring: Ring[A]): Boolean =
    ring.plus(ring.plus(x, y), z) == ring.plus(x, ring.plus(y, z))

implicit def orderingList[A](implicit ord: Ordering[A]): Ordering[List[A]] =
    new Ordering[List[A]]:
        def compare(xs: List[A], ys: List[A]) =
            (xs, ys) match
                case (x :: xsTail, y :: ysTail) =>
                    val c = ord.compare(x, y)
                    if (c != 0) c else compare(xsTail, ysTail)
                case (Nil, Nil) => 0
                case (_, Nil)   => 1
                case (Nil, _)   => -1


// Implicit conversions.
// How to implement some useful built-in mechanisms:

// type coercion:
enum Json:
    case JNumber(value: BigDecimal)
    case JString(value: String)
    case JBoolean(value: Boolean)
    case JArray(value: List[Json])
    case JObject(fields: (String, Json)*)

object Json:
    import scala.language.implicitConversions
    implicit def stringToJson(s: String): Json = JString(s)
    implicit def intToJson(n: Int): Json = JNumber(n)

    def obj(fields: (String, Json)*): Json = JObject(fields: _*)

Json.obj("name" -> "Paul", "age" -> 42)

// extension methods
case class Duration(value: Int, unit: TimeUnit)

object Duration:
    object Syntax:
        import scala.language.implicitConversions
        implicit class HasSeconds(n: Int):
            def seconds: Duration = Duration(n, TimeUnit.SECONDS)

import Duration.Syntax.*
val delay = 15.seconds
// is converted to
val delayy = new HasSeconds(15).seconds
// and is much better than
val delayyy = Duration(15, TimeUnit.SECONDS)
