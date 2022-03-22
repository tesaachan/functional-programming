// Exercise Session 2
// This week we will work on playing with functions as values.



// Question 1
// Define the function flip. It takes a function and returns the same function, but with the arguments flipped.

def flip(f: (Int, Double) => Int): (Double, Int) => Int = 
    (x: Double, y: Int) => f(y, x)



// Question 2

// Question 2.1
// Define the identity function for integers, which, given an Int, returns it.

val id: Int => Int = (x: Int) => x

// Question 2.2
// Define the compose function, that, given 2 functions f, g, returns a function that composes them, i.e., f ∘ g.

def compose(f: Int => Int, g: Int => Int): Int => Int = 
    (x: Int) => f(g(x))

// Question 2.3
// Define the function repeated, which takes a function and repeatedly applies it n times (n ≥ 0).

def repeated(f: Int => Int, n: Int): Int => Int = 
    (x: Int) => if n != 0 then f(repeated(f, n - 1)(x)) else x


// Hint: What values should be returned by repeated(x => x + 1, 0) and repeated(x => x + 1, 3)?



// Question 3

// Question 3.1
// Define the function curry2, that curries a two arguments function. That is, curry2(f) = g such that f(x, y) == (g(x))(y)

def curry2(f: (Double, Int) => Boolean): Double => (Int => Boolean) = 
    (x: Double) => (y: Int) => f(x, y)


// Hint: what should curry2((x, y) => x < y)(1.0) return?

// Question 3.2
// Define the function uncurry2. It takes a curried function, and creates a two-argument function.

def uncurry2(f: Double => Int => Boolean): (Double, Int) => Boolean = 
    (x: Double, y: Int) => f(x)(y)



// Question 4
// Write a function fixedPoint with the following signature:

// def fixedPoint(f: Int => Int): Int => Int


// The function takes a function f and returns a function that maps an integer into the fixed point of f that is obtained by iterating f some finite number of times starting from the initial value.
// A value x is a fixed point of f if f(x) == x.
// For each of the following expressions, indicate whether it terminates, and if so, what is the value returned:

// fixedPoint(x => x/2)(4)
// fixedPoint(id)(123456)
// fixedPoint(x => x + 1)(0)
// fixedPoint(x => if (x % 10 == 0) x else x + 1)(35)
// fixedPoint((x: Int) => x / 2 + 5)(20)

def fixedPoint(f: Int => Int): Int => Int = 
    def iterate(next: Int): Int =
        val applied = f(next)
        if applied == next then next
        else iterate(applied)
    (x: Int) => iterate(x)



// Question 5

// Question 5.1
// Write the sum function with the following signature:

def sum(f: Int => Int)(a: Int, b: Int): Int =
    def iterate(i: Int, acc: Int): Int =
        if i > b then acc else iterate(i + 1, acc + f(i))
    iterate(a, 0)


// Which returns the sum of f(i) where i ranges from a to b.
// Bonus point: Can your implementation be tail recursive ?

// Question 5.2
// Write the quadratic function with the following signature:

def quadratic(c: Int): Int => Int = 
    (x: Int) => (x - c) * (x - c)


// Which returns a function that takes an integer x as argument and returns (x - c)².

// Question 5.3
// Using the above functions, define the function quad3Integrate which, given two integers a and b, computes the sum of (i - 3)²  where i ranges from a to b.

val quad3Integrate: (Int, Int) => Int = 
    sum(quadratic(3))




def f(a: Int, b: Double) = a + b.toInt

def expTwo(a: Int) = a * a
def minus(a: Int) = -a
val minusedExpTwo = compose(minus, expTwo)

def somef(x: Double, y: Int) = x.toInt == y
val eqtopi = curry2(somef)(3.14)
def uncurEqtopi = uncurry2(curry2(somef))

flip(f)(3.4, 1)             ==  f(1, 3.4)
id(5)                       ==  5
minusedExpTwo(5)            ==  minus(expTwo(5))
repeated(x => x + 1, 0)(5)  ==  5
repeated(x => x + 1, 3)(5)  ==  8
eqtopi(3)                   ==  true
uncurEqtopi(3.14, 3)        ==  true

fixedPoint(x => x/2)(4)
fixedPoint(id)(123456)
fixedPoint(x => if (x % 10 == 0) x else x + 1)(35)
fixedPoint((x: Int) => x / 2 + 5)(20)

sum(x => x * x)(1, 4)
quadratic(4)(6)
