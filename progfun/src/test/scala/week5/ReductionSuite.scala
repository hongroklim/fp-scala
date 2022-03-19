package week5

import Reduction._

class ReductionSuite extends munit.FunSuite {
  test("map") {
    assertEquals(mapFun(List(1,2,3), (x: Int) => x * 2), List(2,4,6))
  }

  test("length") {
    assertEquals(lengthFun(List(1,2,3)), 3)
  }
}
