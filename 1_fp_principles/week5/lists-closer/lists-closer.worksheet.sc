val xs = 1 :: 2 :: 3 :: Nil
val ys = 4 :: 5 :: Nil

// lists decomposition:

xs.head
xs.tail
xs.isEmpty
xs match
    case x :: y :: _ => x + y
    case _ => 0

// lists methods:

xs.length           // list's length
xs.last             // last element
xs.init             // list of all elements except the last one
xs.take(2)          // list of the first N elements or all elems if its lenth < N
xs.drop(2)          // rest of the list after taking N elements
xs(1)               // (or xs.apply(1)) the element of the list at index N

// creating new lists:

xs ++ ys            // list of all elems of xs followed by all elems of ys
xs.reverse          // all xs elems in reversed order
xs.updated(1, 9)    // the same list as xs, except at index N where it contains X

// finding elements

xs.indexOf(3)       // gives the index of the first elem in xs equal to X
                    // or -1 if X does not appear in xs
xs.contains(4)      // same as xs.indexOf(x) >= 0



// These methods implementation and complexity

// last:            O(xs.length)

def last[T](xs: List[T]): T = xs match
    case List() => throw Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)

// init:            O(xs.length)

def init[T](xs: List[T]): List[T] = xs match
    case List() => throw Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)

// concatenation:   O(xs.length)

extension [T](xs: List[T])
    def ++ (ys: List[T]): List[T] = xs match
        case Nil => ys
        case x :: xs1 => x :: (xs1 ++ ys)

// reverse:        O(xs.length * xs.length)     but we can do better

extension [T](xs: List[T])
    def reverse: List[T] = xs match
        case Nil => Nil
        case y :: ys => ys.reverse ++ List(y)

// removeAt:

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match
    case Nil => xs
    case y :: ys => 
        if n == 0 then ys
        else y :: removeAt(n - 1, ys)

removeAt(1, "abcd".toList)

// flatten:

def flatten(xs: List[Any]): List[Any] =  xs match
    case Nil => Nil
    case y :: ys => y match
        case y: List[Any] => flatten(y) ++ flatten(ys)
        case y: Any => y :: flatten(ys)

flatten(List(List(1, 1), 2, List(3, List(5,8)), 4))
