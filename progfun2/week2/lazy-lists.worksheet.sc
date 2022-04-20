// Lazy Lists

// Laziness: 
// Avoid computing the elements of a sequence 
// until they are needed for the evaluation result (which might be never)

// Lazy lists are similar to lists, 
// but their elements are evaluated only on demand.


// Lazy lists are defined from a constant LazyList.empty and a constructor LazyList.cons:
val xs = LazyList.cons(1, LazyList.cons(2, LazyList.empty))

// or define like the others collections:
LazyList(1, 2, 3)

// The to(LazyList) method on a collection will turn the collection into a lazy list:
(1 to 1000).to(LazyList)

// A function returning (lo until hi).to(LazyList).
// Difference with a List is LazyList.empty instead Nil and cons(a, b) instead a :: b.
def lazyRange(lo: Int, hi: Int): LazyList[Int] =
    if lo >= hi then LazyList.empty
    else LazyList.cons(lo, lazyRange(lo + 1, hi))

// LazyList supports almost all methods of List.

// There is however an alternative operator for cons:   #:: 
// which produces a lazy list.
// #:: can be used in expressions as well as patterns.

val x = 0
x #:: xs == LazyList.cons(x, xs)


// Implementation of Lazy Lists

// As a simplification, we consider for now that lazy lists are only lazy in their tail. 
// head and isEmpty are computed when the lazy list is created.
// This is not the actual behavior of lazy lists, but makes the implementation simpler to understand.

// trait TailLazyList[+A] extends Seq[A]:
//     def isEmpty: Boolean
//     def head: A
//     def tail: TailLazyList[A]

// object TailLazyList:
//     def cons[T](hd: T, tl: => TailLazyList[T]) = new TailLazyList[T]:
//         def isEmpty = false
//         def head = hd
//         def tail = tl
//         override def toString = "LazyList(" + hd + ", ?)"

//     val empty = new TailLazyList[Nothing]:
//         def isEmpty = true
//         def head = throw ju.NoSuchElementException("empty.head")
//         def tail = throw ju.NoSuchElementException("empty.tail")
//         override def toString = "LazyList()"
