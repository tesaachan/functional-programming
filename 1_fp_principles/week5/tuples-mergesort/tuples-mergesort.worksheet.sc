// splitAt(n) function on lists returns two sublists:
// > xs list contains the elems up to the given index
// > ys list contains the elems from the index
//
// the lists are returned in a pair (xs, ys):

extension [T](xs: List[T])
    def splitAt(n: Int) = (xs.take(n), xs.drop(n))

"abcd".toList.splitAt(2)

// pair patterns:

val somePair = ("Scala", 3)
val second1 = somePair._2
val (_, second2) = somePair
val (proglang, version) = somePair


// Tuples

// for small n the tuple type is scala.Tuplen[T1,...,Tn]
// for n > 22 there is also a TupleXXL class
// the fields of a tuple can be accessed with _1, _2, ...

case class Tuple2[+T1, +T2](_1: T1, _2: T2):
    override def toString = "(" + _1 + "," + _2 + ")"



// first MergeSort implementation (for Int lists)

def intmsort(xs: List[Int]): List[Int] =
    val n = xs.length / 2
    if n == 0 then xs
    else
        def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if x < y then x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        end merge
        val (fst, snd) = xs.splitAt(n)
        merge(intmsort(fst), intmsort(snd))


// parameterization of Sort

def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] =
    val n = xs.length / 2
    if n == 0 then xs
    else
        def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x :: xs1, y :: ys1) =>
                if lt(x, y) then x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
        end merge
        val (fst, snd) = xs.splitAt(n)
        merge(msort(fst)(lt), msort(snd)(lt))


val xs = List(-5, 6, 3, 2, 7)
val fruits = List("apple", "pear", "orange", "pineapple")

msort(xs)((x: Int, y: Int) => x < y)
msort(fruits)((x: String, y: String) => x.compareTo(y) < 0)

//simplified

msort(xs)((x, y) => x < y)
