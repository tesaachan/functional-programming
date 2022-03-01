
val tolerance = 0.0001

def is_close_enough(x: Double, y: Double) =
    Math.abs((x-y) / x) < tolerance

def fixed_point(f: Double => Double)(first_guess: Double): Double =
    def iterate(guess: Double): Double =
        val next = f(guess)
        if is_close_enough(guess, next) then next
        else iterate(next)
    iterate(first_guess)

def sqrt(x: Double) = fixed_point(y => (y + x/y) / 2)(1.0)

@main def main() =
    val ans = sqrt(2)
    println(ans)
