// Set

val fruit = Set("apple", "banana", "pear")
val s = (1 to 6).toSet

// Most operations of sequences are also available on sets:

s.map(_ + 2)
fruit.filter(_.startsWith("app"))
s.nonEmpty

// 1. Sets are unordered
// 2. Sets do not have duplicate elems
// 3. The fundamental operation on sets is 'contains':

s.contains(5)


// Nth queens problem:

def isSafe(col: Int, queens: List[Int]): Boolean =
    !checks(col, 1, queens)

def checks(col: Int, delta: Int, queens: List[Int]): Boolean =
    queens match
        case qcol :: others =>
            qcol == col                         // verical check
            || (qcol - col).abs == delta        // diagonal check   
            || checks(col, delta + 1, others)
        case Nil =>
            false

def queen(n: Int) =
    def placeQueens(k: Int): Set[List[Int]] =
        if k == 0 then Set(List())
        else
            for
                queens <- placeQueens(k - 1)
                col <- 0 until n
                if isSafe(col, queens)
            yield col :: queens
    placeQueens(n)
