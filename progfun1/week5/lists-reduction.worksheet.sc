// Reduction of lists

// 'sum' usual recursive implementation:

def sum(xs: List[Int]): Int = xs match
    case Nil     => 0
    case y :: ys => y + sum(ys)

// generic method 'reduceLeft':
// List(x1, ..., xn).reduceLeft(op) = x1.op(x2). ... .op(xn)

def newsum(xs: List[Int]): Int  = (0 :: xs).reduceLeft((x, y) => x + y)
def product(xs: List[Int]): Int = (1 :: xs).reduceLeft((x, y) => x * y)

// shortet way:

def elsesum(xs: List[Int]): Int     = (0 :: xs).reduceLeft(_ + _)
def elseproduct(xs: List[Int]): Int = (1 :: xs).reduceLeft(_ * _)

// 'foldLeft' is like resuceLeft but takes an accumulator
// List(x1, ..., xn).foldLeft(z)(op) = z.op(x1).op ... .op(xn)

def moresum(xs: List[Int])     = xs.foldLeft(0)(_ + _)
def moreproduct(xs: List[Int]) = xs.foldLeft(1)(_ * _)


// reduceLeft and foldLeft implementation:


extension [T, U](r: List[T])
    def foldLeft(z: U)(op: (U, T) => U): U = r match
        case Nil     => z
        case x :: xs => xs.foldLeft(op(z, x))(op)

    def reduceLeft(op: (T, T) => T): T = r match
        case Nil     => throw IllegalAccessException("Nil.reduceLeft")
        case x :: xs => xs.foldLeft(x)(op);


// foldLeft and reduceLeft unfold on trees that lean to the left.
// there are foldRight and reduceRight which produce trees which lean to the right:

// List(x1, ..., xn).reduceRight(op) = x1.op(x2.op(... x{n-1}.op(xn)...
// List(x1, ..., xn).foldRight(z)(op) = x1.op(x2.op(... x.op(z) ...))


extension [T, U](r: List[T])
    def foldRight(z: U)(op: (T, U) => U): U = r match
        case Nil     => z
        case x :: xs => op(x, xs.foldRight(z)(op))

    def reduceRight(op: (T, T) => T): T = r match
        case Nil          => throw IllegalAccessException("Nil.reduceRight") 
        case x :: Nil => x
        case x :: xs  => op(x, xs.reduceRight(op))


// for associative and commutative operators Left and Right are the same.
// But sometimes only one are suitable:

def concat[T](xs: List[T], ys: List[T]): List[T] =
    xs.foldRight(ys)(_ :: _)

// In this function foldLeft is not allowed because
// for foldLeft op is (U, T) => U and here U is List[T],
// so it will List[T] :: T and it is not type correct.
// But for foldRight op is (T, U) => U, so it will T :: List[T] that is OK.


// Now using foldLeft we can develop a func for reversing lists which has O(xs.length):

def reverse[T](xs: List[T]): List[T] = 
    xs.foldLeft(List[T]())((xs, x) => x :: xs)


// 'map' and 'length' implementation:

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    xs.foldRight(List[U]())((y, ys) => f(y) :: ys)

def lengthFun[T](xs: List[T]): Int =
    xs.foldRight(0)((_, n) => 1 + n)
