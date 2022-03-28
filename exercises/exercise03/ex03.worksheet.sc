// Exercise Session 3
// This week we will play with genericity and object-oriented programming concepts.
// A binary search tree is a binary tree such that, for a node, all elements in the left sub-tree are smaller than the element at the node, and all elements in the right sub-tree are greater than the element at the node. Therefore, binary search trees do not contain duplicate elements.
// Because we want to build a generic tree structure, we also need the notion of a comparator, or a less-than-or-equal operator (denoted leq) for two generic elements which satisfies the following properties:

// Transitivity:   leq(a, b) && leq(b, c) => leq(a, c).
// Reflexivity:    leq(a, a) is true.
// Anti-symmetry:  leq(a, b) && leq(b, a) => a == b.
// Totality:       either leq(a, b) or leq(b, a) is true (or both).

// Note that the above defines a total order.
// Here is the structure we will be using for implementing these trees:

// trait Tree[T]
case class EmptyTree[T](leq: (T, T) => Boolean) extends Tree[T]
case class Node[T](left: Tree[T], elem: T, right: Tree[T], leq: (T, T) => Boolean) extends Tree[T]


// For consistency, all subtrees must contain the same leq parameter.
// Creating an empty binary tree for integers can be then done as follows:

val intLeq: (Int, Int) => Boolean = (x, y) => x <= y
val emptyIntTree: Tree[Int] = new EmptyTree(intLeq)



// Question 1
// Given only leq for comparison, how can you test for equality? How about strictly-less-than?

def eq[T](a: T, b: T, leq: (T, T) => Boolean) = leq(a, b) && leq(b, a)
def less[T](a: T, b: T, leq: (T, T) => Boolean) = !leq(b, a)



// Question 2
// Define the size method on Tree[T], which returns its size, i.e. the number of Nodes in the tree.

trait Tree[T]:
    def size: Int = this match
        case EmptyTree(_) => 0
        case Node(left, _, right, _) => left.size + right.size + 1

        

// Question 3
// Define the add method, that adds an element to a Tree[T], and returns the resulting tree:

    def add(t: T): Tree[T] = this match
        case EmptyTree(leq) => Node(EmptyTree(leq), t, EmptyTree(leq), leq)
        case Node(left, elem, right, leq) =>
            if leq(t, elem) then 
                if leq(elem, t) then this 
                else Node(left.add(t), elem, right, leq)
            else Node(left, elem, right.add(t), leq)


// Remember that trees do not have duplicate values. If t is already in the tree, the result should be unchanged.



// Question 4
// Define the function toList, which returns the sorted list representation for a tree. For example, emptyIntTree.add(2).add(1).add(3).toList should return List(1, 2, 3)

    def toList: List[T] = 
        def loop(cur: Tree[T]) = cur match
            case EmptyTree(_) => Nil
            case Node(l, elem, r, leq) => l.toList ::: (elem :: r.toList)
        loop(this)


// You can use the Nil operator for creating an empty list, and the :: operator for adding a new element to the head of a list: 1 :: List(2, 3) == List(1, 2, 3). You are naturally free to define any auxiliary functions as necessary.



// Question 5
// Define the function sortedList, which takes an unsorted list where no two elements are equal, and returns a new list that contains all the elements of the previous list (and only those), in increasing order.

def sortedList[T](leq: (T, T) => Boolean, ls: List[T]): List[T] =
    def loop(cur: List[T], acc: Tree[T]): Tree[T] = cur match
        case Nil => acc
        case x :: xs => loop(xs, acc.add(x))
    loop(ls, EmptyTree(leq)).toList


// Hint: you might need to define some auxiliary functions.



// Question 6
// If all methods are implemented using pattern matching (i.e. there are no methods implemented in subclasses), can you represent your tree type as an ADT (algebraic data type) using the enum syntax?

enum EnTree[T]:
    case EnEmptyTree(leq: (T, T) => Boolean)
    case EnNode(left: EnTree[T], elem: T, right: EnTree[T], leq: (T, T) => Boolean)

    def size: Int = this match
        case EnEmptyTree(_) => 0
        case EnNode(left, _, right, _) => left.size + right.size + 1

    def add(t: T): EnTree[T] = this match
        case EnEmptyTree(leq) => EnNode(EnEmptyTree(leq), t, EnEmptyTree(leq), leq)
        case EnNode(left, elem, right, leq) =>
            if leq(t, elem) then 
                if leq(elem, t) then this 
                else EnNode(left.add(t), elem, right, leq)
            else EnNode(left, elem, right.add(t), leq)

    def toList: List[T] = 
        def loop(cur: EnTree[T]) = cur match
            case EnEmptyTree(_) => Nil
            case EnNode(l, elem, r, leq) => l.toList ::: (elem :: r.toList)
        loop(this)




emptyIntTree.add(2).add(1).add(3).add(0).toList
sortedList(intLeq, List(2,1,3,0))
