// reccuring patterns for computations on lists:
//
// > transforming   [each elem in a list in a certain way]
// > retrieving     [a list of all elems satisfying a criterion]
// > combining      [the elems of a list using an operator]


// transforming

def scaleList(xs: List[Double], factor: Double): List[Double] = xs match
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)

// study version of map
extension [T](xs: List[T])
    def map[U](f: T => U): List[U] = xs match
        case Nil => Nil
        case x :: xs => f(x) :: xs.map(f)

// new scaleList using map func
def newScaleList(xs: List[Double], factor: Double): List[Double] =
    xs.map(x => x * factor)

def squareList(xs: List[Int]): List[Int] = xs.map(x => x * x)


// retrieving

def posElems(xs: List[Int]): List[Int] = xs match
    case Nil => xs
    case y :: ys => if y > 0 then y :: posElems(ys) else posElems(ys)

// study version of filter
extension [T](xs: List[T])
    def filter(p: T => Boolean): List[T] = xs match
        case Nil => xs
        case x :: xs => if p(x) then x :: xs.filter(p) else xs.filter(p)

// new posElems
def newPosElems(xs: List[Int]): List[Int] = xs.filter(x => x > 0)

// variations of filter:

val xs = List(-2, -1, 0, 1, 2)

xs.filterNot(x => x > 0)    // same as xs.filter(x => !p(x))
xs.partition(x => x > 0)    // same as (xs.filter(p), xs.filterNot(p))
xs.takeWhile(x => x < 0)    // the longest prefix of elems satisfying p
xs.dropWhile(x => x < 0)    // removes all elems satisfying p
xs.span(x => x < 0)         // same as (xs.takeWhile(p), xs.dropWhile(p))


// function pack:
// packs consecutive duplicates of list elements into sublists

def pack[T](xs: List[T]): List[List[T]] = xs match
    case Nil => Nil
    case y :: ys => 
        val (same, rest) = ys.span(z => z == y)
        (y :: same) :: pack(rest)
    
pack("aaabcca".toList)

// function encode:
// using pack produces the run-length encoding of a list

def encode[T](xs: List[T]): List[(T, Int)] = 
    pack(xs).map(x => (x.head, x.length))

encode("aaabcca".toList)
