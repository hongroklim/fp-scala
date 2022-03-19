package week5

class MergeSortSuite extends munit.FunSuite {
  import week5.MergeSort._
  
  test("merge sort") {
    assertEquals(msort(List(1,5,3,6,2,4)), List(1,2,3,4,5,6))
  }

  test("generalized merge sort") {
    assertEquals(genMsort(List(1,5,3,6,2,4))((x: Int, y: Int) => x < y), List(1,2,3,4,5,6))
  }

  test("merge sort of string") {
    val strList = List("a", "d", "b", "c")

    assertEquals(genMsort(strList)((x: String, y: String) => x.compareTo(y) < 0), List("a", "b", "c", "d"))
  }
  
  test("implicited merger sort") {
    assertEquals(implMsort(List(1,5,3,6,2,4)), List(1,2,3,4,5,6))
  }
}
