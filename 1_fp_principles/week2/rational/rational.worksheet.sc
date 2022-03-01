class Rational(x: Int, y: Int):
    require(y > 0, "denominator must be positive")
    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
        if b == 0 then a else gcd(b, a % b)

    val numer = x
    val denom = y

    def add(r: Rational) =
        Rational(numer * r.denom + r.numer * denom,
                    denom * r.denom)

    def mul(r: Rational) =
        Rational(numer * r.numer, denom * r.denom)

    def neg = Rational(-numer, denom)

    def sub(r: Rational) = add(r.neg)

    def less(that: Rational): Boolean =
        numer * that.denom < that.numer * denom

    def max(that: Rational): Rational =
        if this.less(that) then that else this

    override def toString = s"${numer / gcd(x.abs, y)}/" +
                                s"${denom / gcd(x.abs, y)}"
end Rational

extension (r: Rational)
    infix def min(s: Rational): Rational = if s.less(r) then s else r
    def abs: Rational = Rational(r.numer.abs, r.denom)

extension (x: Rational)
    def + (y: Rational): Rational = x.add(y)
    def - (y: Rational): Rational = x.sub(y)
    def * (y: Rational): Rational = x.mul(y)
    def < (y: Rational): Boolean = x.less(y)


val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.add(y).mul(z)
x.neg
y.sub(x)
x.sub(y).sub(z)
Rational(-2).abs
Rational(4, 5).min(Rational(1, 4))
x - y - z
// Rational(1, 3) < (Rational(2, 3))
Rational(4, 5) min (Rational(2, 4))
