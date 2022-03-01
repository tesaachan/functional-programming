def product(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 1
    else f(a) * product(f)(a + 1, b)

def product_of_squares = product(x => x*x)

def fact(n: Int) = product(x => x)(0, n)


def map_reduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int) =
    def recur(a: Int): Int =
        if a > b then zero
        else combine(f(a), recur(a+1))
    recur(a)


def sum(f: Int => Int) = map_reduce(f, (x, y) => x + y, 0)

def new_product(f: Int => Int) = map_reduce(f, (x, y) => x * y, 1)

@main def main() =
    val ans1 = product_of_squares(2,3)
    val ans2 = sum(x => x*x)(1, 3)
    val ans3 = new_product(x => x)(2, 4)
    println(ans3)
