// Vectors

// Vectors are 32 elems collection. If vector has more than 32 elems,
// then it becomes a vector consisting of 32 vectors, so now it has 32*32=1024 elems
// and so on up to 5 layouts, so maximum vector can have 2^25 elems.
// And to change an elem it is changed in the 32-size vector below and in its parent vectors
// up to the root.

// Vector supports the same operations as List, but except 'cons' there are:
// x +: xs      add an elem to the left of a vector
// xs :+ x      add an elem to the right


val nums = Vector(1, 2, 3);
0 +: nums :+ 4 :+ 5

// The superclass of Vector and List is Seq – the subclass of Iterable.
// Array class is from Java but it has automatic cast to Seq.
// String also can be converted to Seq.
// Other Iterable's subclasses are Set and Map.

val zs: Array[Int] = Array(1, 2, 3)
zs.map(x => 2 * x)

val ys: String = "Hello world!"
ys.filter(_.isUpper)


// Ranges

// Simple kind of sequence that's represented as evenly spaced integers.
// Range is a single object with three fields: lower bound, upper bound, step value.

val r: Range = 1 until 5    // 1 2 3 4
val s: Range = 1 to 5       // 1 2 3 4 5
1 to 10 by 3                // 1 4 7 10
6 to 1 by -2                // 6 4 2



val xs: List[Int] = List(1, 2, 3, 4, 5)
val ts: List[List[Int]] = List(List(1,2), List(3,4))
val ps = xs.zip(ys)
def p(x: Int): Boolean = x % 2 == 0
def f(x: List[Int]): IterableOnce[Int] = x.map(_ * 2)

// Sequence operations (for all Seq members):

xs.exists(p)
xs.forall(p)

xs.zip(ys)
ps.unzip

ts.flatMap(f)

xs.sum
xs.product

xs.max
xs.min



// Example: to list all combinations of numbers x and y where x is drawn
// from 1 to M and y is drawn from 1 to N:

val M = 3
val N = 2

(1 to M).flatMap(x => (1 to N).map(y => (x, y)))

// Example: scalar product

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    xs.zip(ys).map((x, y) => x * y).sum

// or

def elseScalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    xs.zip(ys).map(_ * _).sum


def isPrime(n: Int): Boolean =
    if n < 4 then true else
    (2 to math.sqrt(n).toInt).forall(n % _ != 0)

isPrime(11)
