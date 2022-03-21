import scala.annotation.tailrec
// Exercise Session 1
// We will work on tail recursion in this session.



// Question 1: Factorial
// Recall the factorial function that you saw in class

def factorial(n: Int): Int = if (n <= 0) 1 else n * factorial(n - 1)

// Define a tail recursive version of it

def factorialRec(n: Int): Int =
    @tailrec
    def fact(n: Int, acc: Int): Int = 
        if n == 1 then acc else fact(n - 1, acc * n)
    fact(n, 1)

// What would be the advantage of making fact an inner function to factorial?
// It would encapsulate this function inside, so it becomes one logical element.



// Question 2: Sum of elements on a list
// Define a function that takes a list of integers and sums them. You can use the functions head, tail, and isEmpty on lists, as you have seen for your homework.

def sumList(ls: List[Int]): Int = 
    @tailrec
    def sum(rest: List[Int], acc: Int): Int = 
        if rest.isEmpty then acc else sum(rest.tail, acc + rest.head)
    sum(ls, 0)

// Convert your definition into a tail-recursive one.



// Question 3: Fast exponentiation
// Fast exponentiation is a technique to optimize the exponentiation of numbers:

// b²ⁿ = (b²)ⁿ = (bⁿ)²
// b²ⁿ⁺¹ = b * b²ⁿ

// Define a function that implements this fast exponentiation. Can you define a tail recursive version as well?

def fastExp(base: Int, exp: Int): Int =
    if exp == 0 then 1
    else if exp % 2 == 1 then base * fastExp(base, exp - 1)
    else 
        val b = fastExp(base, exp / 2)
        b * b

def fastExpTailRec(base: Int, exp: Int): Int =
    @tailrec
    def recExp(curBase: Int, curExp: Int, acc: Int): Int =
        if curExp == 0 then acc
        else if curExp % 2 == 1 then recExp(curBase, curExp - 1, curBase * acc)
        else recExp(curBase * curBase, curExp / 2, acc)
    recExp(base, exp, 1)



// Question 4: Tail recursive Fibonacci
// Define a function that computes the nth Fibonacci number. Can you define a tail recursive version as well? The Fibonacci recurrence is given as follows:

// fib(n) = n | n = 0, 1
// fib(n) = fib(n - 1) + fib(n - 2) | otherwise

def fibonacci(n: Int): Int =
    if n == 0 then 0
    else if n == 1 then 1
    else fibonacci(n-1) + fibonacci(n-2)

def fibonacciTailRec(n: Int): Int =
    def recFib(cur: Int, a: Int, b: Int): Int =
        if cur == n then a + b
        else recFib(cur + 1, a + b, a)
    recFib(2, 1, 0)



factorial(5) == factorialRec(5)
sumList(List(2,3,4,5,2)) == 16
fastExp(2, 12) == fastExpTailRec(2, 12)
fibonacci(5) == fibonacciTailRec(5)

