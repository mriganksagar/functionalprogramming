package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.runtime.stdLibPatches.language.`3.0`
import org.scalacheck.rng.Seed

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[A]
    h <- frequency(
      1 -> const(empty),
      9 -> genHeap
    )
  } yield insert(i, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  def getLength(h: H): Int =
    if isEmpty(h) then 0
    else 1 + getLength(deleteMin(h))

  def toList(sub_heap: H): List[Int] =
    if isEmpty(sub_heap) then Nil
    else findMin(sub_heap) :: toList(deleteMin(sub_heap))

  property(
    "Min1: Inserting any number to a empty heap will give same number as Min"
  ) = forAll { (i: Int) =>
    i == findMin(insert(i, empty))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Custom1: Non-empty heaps meld to make non-empty heap") = forAll {
    (h1: H, h2: H) =>
      isEmpty(meld(h1, h2)) == (isEmpty(h1) && isEmpty(h2))
  }

  property(
    "Custom2: Insert min Integer to any heap should give MinInt with findMinumum"
  ) = forAll { (h: H) =>
    findMin(insert(Int.MinValue, h)) == Int.MinValue
  }

  property(
    "Custom3: Meld two Heaps, and their length should add together"
  ) = forAll { (h1: H, h2: H) =>

    val h_meld = meld(h1, h2)
    getLength(h_meld) == getLength(h1) + getLength(h2)
  }

  property(
    "Custom4: On deleting the length changes by -1"
  ) = forAll { (h: H) =>
    if isEmpty(h) then true else getLength(h) == getLength(deleteMin(h)) + 1
  }

  property("Custom5: Melding should preserve elements") = forAll {
    (ha: H, hb: H) =>
      val ha_l = toList(ha)
      val hb_l = toList(hb)

      toList(meld(ha, hb)).sorted == (ha_l ++ hb_l).sorted
  }
  // Properties mentioned in Coursera Exercise to implement
  property(
    "P1: Inserting two elements into empty heap should give findMin as smaller of them"
  ) = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == a.min(b)
  }

  property(
    "P2: Insert a element into Empty-heap and delete same to get Empty again"
  ) = forAll { (i: Int) =>
    val h = insert(i, empty)
    deleteMin(h) == empty
  }

  property("P3: Should get a sorted sequence") = forAll { (h: H) =>
    val list_heap = toList(h)
    if list_heap.length >= 2 then
      list_heap.sliding(2).forall { case Seq(a, b) => a <= b }
    else true
  }

  property(
    "P4:Either of Min of two Non-empty heaps is the Min of melded Heap"
  ) = forAll { (ha: H, hb: H) =>
    val h_meld = meld(ha, hb)
    (isEmpty(ha), isEmpty(hb)) match
      case (true, true)  => isEmpty(h_meld)
      case (true, false) => findMin(hb) == findMin(h_meld)
      case (false, true) => findMin(ha) == findMin(h_meld)
      case (false, false) => {
        val min_meld = findMin(h_meld)
        min_meld == findMin(ha) || min_meld == findMin(hb)
      }
  }

object checkDemo extends App with IntHeap with BinomialHeap {
  val genHeap_0: Gen[H] = for {
    i <- arbitrary[A]
    h <- frequency(
      1 -> const(empty),
      99 -> genHeap_0
    )
  } yield insert(i, h)

  lazy val genHeap: Gen[H] = arbitrary[A].flatMap(i =>
    frequency(
      1 -> const(empty),
      99 -> genHeap
    ).map(h => insert(i, h))
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  def getLength(h: H): Int =
    if isEmpty(h) then 0
    else 1 + getLength(deleteMin(h))

  (1 to 10).foreach { i =>
    val seed = Seed.random() // Create a seed
    println(seed)
    val heap =
      genHeap(Gen.Parameters.default, seed) // Generate with this seed
    println(s"Heap $i: ${getLength(heap.get)}")
  }
}
