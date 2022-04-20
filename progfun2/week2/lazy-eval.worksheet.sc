// Lazy evaluation

// All lazy vals don't recompute each time they are called.
// The result of the first evaluation is stored for re-using instead of recomputing.
// It is okay since purely func lang expressions produces the same res each time.


// The real LazyList implementation:

// class LazyList[+T](init: => State[T]):
//     lazy val state: State[T] = init

// enum State[T]:
//     case Empty
//     case Cons(hd: T, tl: LazyList[T])


// Computing with Infinite Sequences

def from(n: Int): LazyList[Int] = n #:: from(n+1)

// The list of all natural numbers:
val nats = from(0)

// The list of all multiples of 4:
nats.map(_ * 4)

// The sieve of Eratosthenes:
def sieve(s: LazyList[Int]): LazyList[Int] =
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))

val primes = sieve(from(2))

// First 4 prime numbers:
primes.take(4).toList


// Back to Square Roots

// Prev square roots algorithm used isGoodEnough test to terminate the iteration.
// With lazy lists we can now express the concept of a sqrt sequence:

def sqrtSeq(x: Double): LazyList[Double] =
    def improve(guess: Double) = (guess + x/guess) / 2
    lazy val guesses: LazyList[Double] = 1 #:: guesses.map(improve)
    guesses

// With higher index of the sequence we get the higher precision result:
sqrtSeq(2)(2)
sqrtSeq(2)(20)

// And now we can add isGoodEnough:
def isGoodEnough(guess: Double, x: Double) =
    ((guess * guess - x) / x).abs < 0.0001

sqrtSeq(2).filter(isGoodEnough(_, 2))
