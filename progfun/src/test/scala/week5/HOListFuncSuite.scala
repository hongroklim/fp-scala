package week5

import HOListFunc._

class HOListFuncSuite extends munit.FunSuite {
  test ("pack") {
    val eval = pack(List("a", "a", "a", "b", "c", "c", "a"))
    val expected = List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
    assertEquals(eval, expected)
  }

  test ("encode") {
    val eval = encode(List("a", "a", "a", "b", "c", "c", "a"))
    val expected = List(("a", 3), ("b", 1), ("c", 2), ("a", 1))
    assertEquals(eval, expected)
  }
  
  test ("map encode") {
    val eval = mapEncode(List("a", "a", "a", "b", "c", "c", "a"))
    val expected = List(("a", 3), ("b", 1), ("c", 2), ("a", 1))
    assertEquals(eval, expected)
  }
}
