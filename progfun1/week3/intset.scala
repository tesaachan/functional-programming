abstract class IntSet:
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(s: IntSet): IntSet

object Empty extends IntSet:
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
    def union(s: IntSet): IntSet = s

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:

    def contains(x: Int): Boolean =
        if x < elem then left.contains(x)
        else if x > elem then right.contains(x)
        else true

    def incl(x: Int): IntSet =
        if x < elem then NonEmpty(elem, left.incl(x), right)
        else if x > elem then NonEmpty(elem, left, right.incl(x))
        else this

    def union(s: IntSet): IntSet =
        left.union(right).union(s).incl(elem)

end NonEmpty

object IntSet:
    def apply = Empty
    def apply(x: Int): IntSet = Empty.incl(x)
    def apply(x: Int, y: Int): IntSet = Empty.incl(x).incl(y)


@main def main(name: String, age: Int): Unit =
    println(s"Hello, $name, who is $age years old")
    val A = Empty.incl(3)
    val B = A.incl(4)
    val C = B.incl(2)
    println(C.contains(3))
    println(C.contains(1))
