package week5

class ListExtensionSuite extends munit.FunSuite {
  import week5.ListExtension._

  test("remove at") {
    assertEquals(removeAt(1, List('a', 'b', 'c', 'd')), List('a', 'c', 'd'))
  }

  test("remove at advanced") {
    assertEquals(removeAt2(1, List('a', 'b', 'c', 'd')), List('a', 'c', 'd'))
  }

  test("flatten") {
    val answ = List(1, 1, 2, 3, 5, 8)
    assertEquals(flatten(List(List(1, 1), 2, List(3, List(5, 8)))), answ)
  }

} 
