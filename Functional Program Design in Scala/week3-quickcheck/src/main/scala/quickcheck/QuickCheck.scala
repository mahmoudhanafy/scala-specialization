package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = Gen.frequency(
    (1, Gen.const(empty)),
    (5, for {
      x <- arbitrary[Int]
      a <- genHeap
    } yield insert(x, a)
    )
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("minimum should be fixed after inserting same minimum value") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Heap should get minimum of 2 values") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, insert(b, empty))
    findMin(heap) == Math.min(a, b)
  }

  property("deleting minimum from heap with one element should result in empty heap") = forAll { a: Int =>
    val heap = insert(a, empty)
    isEmpty(deleteMin(heap))
  }

  def getHeapElements(h: H): List[Int] =
    if (isEmpty(h)) Nil
    else findMin(h) :: getHeapElements(deleteMin(h))

  property("get sorted sequence when deleting all elements from heap") = forAll { h: H =>
    val sortedHeapElements = getHeapElements(h)
    sortedHeapElements == sortedHeapElements.sorted
  }

  lazy val nonEmptyHeapGen = genHeap.suchThat(h => !isEmpty(h))

  property("melding two heaps should keep minimum") = Prop.forAll(nonEmptyHeapGen, nonEmptyHeapGen) { (a: H, b: H) =>
    val minA = findMin(a)
    val minB = findMin(b)
    val combinedHeap = meld(a, b)
    findMin(combinedHeap) == Math.min(minA, minB)
  }

  property("melding empty heap with nonEmpty heap should return the same non empty heap") = forAll(nonEmptyHeapGen) { h =>
    val heap = meld(empty, h)
    getHeapElements(heap) == getHeapElements(h)
  }

  property("melding 2 queues works fine") = forAll { (a: H, b: H) =>
    val combined = meld(a, b)
    val expectedElements = (getHeapElements(a) ++ getHeapElements(b)).sorted
    val combinedElements = getHeapElements(combined)
    combinedElements == expectedElements
  }

}
