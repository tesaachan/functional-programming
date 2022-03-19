// Handling sequences

val a = List(List(1, 2), List(3, 4))
def f(a: List[Int]) = a.map(_ * 2)

// useful laws:

a.flatten     ==  a.foldRight(Seq[Int]())(_ ++ _)
a.flatMap(f)  ==  a.map(f).flatten


// some staff for the next example:

def isPrime(n: Int): Boolean =
    if n < 4 then true else
    (2 to math.sqrt(n).toInt).forall(n % _ != 0)

// Example: given a positive integer n, find all pairs of positive i and j,
// with 1 <= j < i < n such that i + j is prime.

val n = 7

(1 until n)
    .flatMap(i => (1 until i).map(j => (i, j)))
    .filter((x, y) => isPrime(x + y))



// But there is a simpler way with for-expressions.
// For-expressions are similar to loops in imperative langs, except that
// it builds a list of the results of all iterations.

// Syntax of for:   
//    for s yield e
// where s is a sequence of generators and filters,
// and e is an expression whose value is returned by an iteration.

// > Generator is of the form p <- e, where p is a pattern,
//   and e is an expression whose value is a collection.
// > Filter is of the form 'if f' where f is a boolean expression.
// > The sequence must start with a generator
// > If there are several generators in the sequence, the last generators
//   vary faster than the first.

// Example: 

case class Person(name: String, age: Int)
val persons = List(Person("Albert", 21), Person("Michael", 17))


for p <- persons if p.age > 20 yield p.name

// which is equivalent to

persons
    .filter(p => p.age > 20)
    .map(p => p.name)

// Solution of the example previously solved with higher-order functions:

for
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
yield (i, j)

// Exercise: 

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for (x, y) <- (xs.zip(ys)) yield x * y).sum

// Example: sum of the mupltiplication of every elem of xs with every elem of ys:

def somefunc(xs: List[Double], ys: List[Double]) =
    (for x <- xs; y <- ys yield x * y).sum
