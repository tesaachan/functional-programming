// In Scala a 'type class' is a generic trait that comes with
// givens instances for type instances of that trait.

// In the Ordering example, we have given instances for Ordering[Int]
// and Ordering[String]:

trait Ordering[A]:
    def compare(x: A, y: A): Int
    
    extension (x: A)
        def < (y: A): Boolean = compare(x, y) < 0
        def <= (y: A): Boolean = compare(x, y) <= 0
        def > (y: A): Boolean = compare(x, y) > 0
        def >= (y: A): Boolean = compare(x, y) >= 0

object Ordering:
    given Ordering[Int] with
        def compare(x: Int, y: Int): Int =
            if x < y then -1 else if x > y then 1 else 0
    given Ordering[String] with
        def compare(s: String, t: String) = s.compareTo(t)

    given Ordering[Rational] with
        def compare(q: Rational, r: Rational) = 
            val (a, b) = (q.num * r.denom, r.num * q.denom)
            if a < b then -1 else if a > b then 1 else 0

    given listOrdering[A](using ord: Ordering[A]): Ordering[List[A]] with
        def compare(xs: List[A], ys: List[A]) = (xs, ys) match
            case (Nil, Nil) => 0
            case (_, Nil)   => 1
            case (Nil, _)   => -1
            case (x :: xs1, y :: ys1) =>
                val c = ord.compare(x, y)
                if (c != 0) c else compare(xs1, ys1)

    given pairOrdering[A, B](using ord1: Ordering[A], ord2: Ordering[B])
    : Ordering[(A, B)] with
        def compare(x: (A, B), y: (A, B)) =
            val comp1 = ord1.compare(x._1, y._1)
            if comp1 != 0 then comp1 else ord2.compare(x._2, y._2)

// So, type classes provide yet another form of polymorphism:
// the sort method can be called with lists containing elements of any type A
// for which there is a given instance of type Ordering[A].

case class Rational(num: Int, denom: Int)

// Type classes support 'retroactive extension': the ability to extend a data
// type with new operations without changing its original definition.

// In this example, we have added the capability of comparing Rational numbers.


// sort(List(List(1,2,3), List(3,2,1), List(2,1,3)))

// Given instances as listOrdering that take imolicit parameters are conditional:
// * An ordering for lists with elements of type T exists only if 
//   there is an ordering for T

// This sort of "conditional behavior" is best implemented with type classes.
// Normal subtyping and iheritance can not express this:
// a class either inherits a trait or does not.



// Type classes and extenison methods

// Like any trait, a type class trait defines extension methods.
// For example, the Ordering usually contain comparison methods.

// Extension methods on a type class trait are visible whenever
// a given instance for trait is visible.

// There is no need to name and import the Ordering instance
// to get access to the extension method.

// We have an Ordering[T] instance in scope, that's where
// the extension method comes from:

def merge[T: Ordering](xs: List[T], ys: List[T]): List[T] = (xs, ys) match
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x :: xs1, y :: ys1) =>
        if x < y then x :: merge(xs1, ys)
        else y :: merge(xs, ys1)


// Summary

// Type classes provide a way to turn types into values.

// Unlike class extensions, type classes:
// * can be defined at any time without changing existing code
// * can be conditional

// Type classes give rise to a new kind of polymorphism (ad-hoc).

// This means that the a type TC[A] has different implementations
// for different types A.
