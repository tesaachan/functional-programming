object Ordering:
    given Int: Ordering[Int] with
        def compare(x: Int, y: Int): Int =
            if x < y then -1 else if x > y then 1 else 0
    // ot it can be anonymous:
    given Ordering[Double] with
        def compare(x: Double, y: Double): Int = ???
    // ...

def sort[T](xs: List[T])(using ord: Ordering[T]): List[T] = 
    def insert(x: T, rest: List[T]): List[T] = rest match
        case y :: ys => 
            if ord.compare(x, y) < 0 then x :: y :: ys
            else y :: insert(x, ys)
        case Nil => List(x)
    if xs.isEmpty then Nil else insert(xs.head, sort(xs.tail))

sort(List(2,3,1))
sort(List("abac", "bac", "acab"))

// with 'using' when there is exactly one 'obvious' value for a type,
// the compiler can provide that value to us.


given a: Int = 5

def f[T](using v: Int) = s"$v" + "?"
f

def printSorted[T](as: List[T])(using Ordering[T]) = println(sort(as))
// that is the same as:
def otherPrintSorted[T: Ordering](as: List[T]) = println(sort(as))


// Summoning an instance:

// def summon[T](using x: T) = x

summon[Ordering[Int]]
summon[Int] // 5



// Search scope:
// * all the 'given' instances that are visible 
//   (inherited, imported, defined in an enclosing scope)
// * the given instances found in a companion object associated with T

// "Associated with T" means:
// * the companion object of a class itself
// * companion objects associated with any of T's inherited types
// * companion objects associated with any type argument in T
// * if T is an inner class, the outer objects in which it is embedded

trait Foo[T]
trait Bar[T]
trait Baz[T] extends Foo[T]
trait X
trait Y extends X

// Bar[Y] required => the compiler will look into the companion objects: 
// Bar, Y, Foo, X (not Baz)


// Importing 

// Givens can be imported in three ways:

// By-name:
import scala.math.Ordering.Int
// By-type (preferred):
import scala.math.Ordering.{given Ordering[Int]}
import scala.math.Ordering.{given Ordering[?]}
// with a wildcard:
import scala.math.given


// Priorities

// Several givens matching the same type don't generate 'ambiguity error'
// if one is more specific than the other.

// "Given a: A" is more specific than "given b: B" if any:
// * a is in a closer lexical scope than b
// * a is defined in a class or object which is a subclass of the class defining b
// * type A is a genetic instance of type B
// * type A is a subtype of type B

class A[T](x: T)
given universal[T](using x: T): A[T](x)
given specific: A[Int](2)

summon[A[Int]]
// gives 'specific' because it has more concise type.
