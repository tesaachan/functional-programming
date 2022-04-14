import scala.compiletime.ops.int
// Functional random generators

trait Generator[+T]:
    def generate(): T

val integers = new Generator[Int]:
    val rand = java.util.Random()
    def generate() = rand.nextInt()

val booleans = new Generator[Boolean]:
    def generate() = integers.generate() > 0

val pairs = new Generator[(Int, Int)]:
    def generate() = (integers.generate(), integers.generate())

// pairs.generate()


// Implement generators in terms of 'for':

extension  [T, S](g: Generator[T])
    def map(f: T => S) = new Generator[S]:
        def generate() = f(g.generate())

    def flatMap(f: T => Generator[S]) = new Generator[S]:
        def generate() = f(g.generate()).generate()

val newBooleans = for x <- integers yield x > 0
// is equal to 'integers.map(x => x > 0)', which is equal to 
// 'new Generator[Boolean]:
//      def generate() = ((x: Int) => x > 0)(integers.generate())'

def newPairs[T, U](t: Generator[T], u: Generator[U]) = 
    for x <- t; y <- u yield (x, y)
// is equal to
// 't.flatMap(x => u.map(y => (x, y)))' and so on


// Generator examples

def single[T](x: T): Generator[T] = new Generator[T]:
    def generate() = x

def range(lo: Int, hi: Int): Generator[Int] =
    for x <- integers yield lo + x.abs % (hi - lo)

def oneOf[T](xs: T*): Generator[T] =
    for idx <- range(0, xs.length) yield xs(idx)
    
def lists: Generator[List[Int]] =
    for
        isEmpty <- booleans
        list <- if isEmpty then emptyLists else nonEmptyLists
    yield list

def emptyLists = single(Nil)

def nonEmptyLists = 
    for 
        head <- integers
        tail <- lists
    yield head :: tail

enum Tree:
    case Inner(left: Tree, right: Tree)
    case Leaf(x: Int)

def trees: Generator[Tree] = 
    for
        isLeaf <- booleans
        tree <- if isLeaf then leafs else inners
    yield tree

def leafs = for x <- integers yield Tree.Leaf(x)

def inners = 
    for left <- trees; right <- trees yield Tree.Inner(left, right)

trees.generate()


// Random testing

def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit =
    for i <- 0 until numTimes do
        val value = g.generate()
        assert(test(value), s"test failed for $value")
    println(s"passed $numTimes tests")

test(newPairs(lists, lists)) {
    (xs, ys) => (xs ++ ys).length >= xs.length
}
