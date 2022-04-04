package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      e <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(e, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a1: A, a2: A) =>
    val min = ord.min(a1, a2)
    val h = insert(a1, insert(a2, empty))
    findMin(h) == min
  }

  property("empty1") = forAll { (a: A) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("sort1") = forAll { (h: H) =>
    def sortedList(h: H, acc: List[A]): List[A] =
      if (isEmpty(h)) acc
      else sortedList(deleteMin(h), findMin(h) :: acc)

    val list = sortedList(h, List())
    list == list.sorted
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == ord.min(findMin(h1), findMin(h2))
  }
}
