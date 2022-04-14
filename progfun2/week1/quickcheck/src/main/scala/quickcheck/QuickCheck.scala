package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  // lazy val genHeap: Gen[H] = oneOf(
  //   const(empty),
  //   for
  //     x <- arbitrary[Int]
  //     h <- oneOf(const(empty), genHeap)
  //   yield insert(x, h)
  // )

  lazy val genHeap: Gen[H] = 
    for
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    yield insert(x, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  def getSortedSeq(h: H) =
    def getSeq(rest: H, seq: List[Int]): List[Int] =
      if isEmpty(rest) then seq
      else getSeq(deleteMin(rest), findMin(rest) :: seq)
    getSeq(h, Nil)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, 
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("gen2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val m = if a < b then a else b
    findMin(h) == m
  }

  // If you insert an element into an empty heap, 
  // then delete the minimum, the resulting heap should be empty.
  property("gen3") = forAll { (a: Int) => 
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // Given any heap, you should get a sorted sequence of elements when continually 
  // finding and deleting minima.
  property("gen4") = forAll { (h: H) =>
    val seq = getSortedSeq(h)
    (for i <- 1 until seq.length if seq(i-1) < seq(i) yield false).isEmpty
  }
  
  // Finding a minimum of the melding of any two heaps 
  // should return a minimum of one or the other.
  property("gen5") = forAll { (h1: H, h2: H) => 
    val melt = meld(h1, h2)
    (isEmpty(h1), isEmpty(h2)) match
      case (true, true) => true
      case (true, false) => findMin(melt) == findMin(h2)
      case (false, true) => findMin(melt) == findMin(h1)
      case (false, false) => 
        findMin(melt) == findMin(h1) || findMin(melt) == findMin(h2)
  }

  // Own prop: 
  // Meld h1 and h2 to h. Take min from h1 and add to h2 then meld them and compare with h.
  property("gen6") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val min = findMin(h1)
    val melt = meld(deleteMin(h1), insert(min, h2))
    getSortedSeq(h) == getSortedSeq(melt)
  }
