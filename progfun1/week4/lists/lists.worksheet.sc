val fruit = List("orange", "apple", "pear")
val nums  = List(1, 2, 3, 4)
val diag  = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
val empty = List()

/*
 *  differences from arrays:
 *  > lists are immutable – the elements can not be changed
 *  > lists are recursive (cons), while arrays are flat
 */

val also_diag: List[List[Int]]  = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

val fruit1 = "orange" :: ("apple" :: ("pear" :: Nil))
val num1 = 1 :: (2 :: (3 :: (4 :: Nil)))
val empty1 = Nil

val num2 = 1 :: 2 :: 3 :: 4 :: Nil

fruit.head
fruit.tail.head
diag.head

/*
 *  pattern matching with lists:
 *  > Nil               the Nil constant
 *  > p :: ps           a list with a head matching p and a tail mathcing ps
 *  > List(p, ..., pn)  same as p :: ... :: pn :: Nil
 */

def isort(xs: List[Int]): List[Int] = xs match
    case List() => List()
    case y :: ys => insert(y, isort(ys))

def insert(x: Int, xs: List[Int]): List[Int] = xs match
    case List() => List(x)
    case y :: ys => 
        if x > y then y :: insert(x, ys) else x :: xs

val alist = List(7, 9, 2, 3)
val sorted_alist = isort(alist)