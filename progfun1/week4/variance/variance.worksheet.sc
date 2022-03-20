import javax.smartcardio.Card
/*
 *  Shortly, a type that accepts mutations of its elements 
 *  should not be covariant.
 *  
 *  But immutable types can be covariant, if some conditions on
 *  methods are met.
 */

/*
 *  Say C[T] is a parameterized type and A and B are A <: B
 *  There is 3 possible relations:
 *  
 *  C[A] <: C[B]        C is covariant
 *  C[A] >: C[B]        C is contrvariant
 *  no suptype          C is nonvariant
 */

/*
 *  Declaring the variance of a type:
 *  
 *  class C[+A]         C is covariant
 *  class C[-A]         C is contravariant
 *  class C[A]          C is nonvariant
 */

// Example:
// In this case FtoO <: AtoF according to LSP

trait Fruit
class Apple extends Fruit
class Orange extends Fruit

type FtoO = Fruit => Orange
type AtoF = Apple => Fruit

/*  The rule for subtyping between function types:
 *  if A2 <: A1 and B1 <: B2, then 
 *     A1 => B1 <: A2 => B2
 *      
 *  It means functions are contravariant in their argument type and
 *  covariant in their result type.
 * 
 */

// 'Function' trait definition:
    
trait Function[-T, +U]:
    def apply(x: T): U

/*  Roughly,
 *
 *  covariant types only in return
 *  contravariant types only in arugments
 *  invariant anywhere
 */ 


// 'List' definition for all cases seen so far:

trait List[+T]:

    def isEmpty = this match
        case Nil => true
        case _ => false

    override def toString =
        def recur(prefix: String, xs: List[T]): String = xs match
            case x :: xs1 => s"$prefix$x${recur(", ", xs1)}"
            case nil => ")"
        recur("List(", this)
    
    def prepend [U >: T] (elem: U): List[U] = ::(elem, this)

case class ::[+T](head: T, tail: List[T]) extends List[T]
case object Nil extends List[Nothing]

// extension [T](x: T) def :: (xs: List[T]): List[T] = ::(x, xs)

// object List:
//     def apply() = Nil
//     def apply[T](x: T) = x :: Nil
//     def apply[T](x1: T, x2: T) = x1 :: x2 :: Nil
//  ...

def f(xs: List[Apple], x: Orange) = xs.prepend(x)
// result type is List[Fruit]

val a1 = new Apple
val a2 = new Apple
val o = new Orange

f(::(a1, Nil), o)
