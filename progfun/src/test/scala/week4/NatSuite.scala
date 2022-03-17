package week4

class NatSuite extends munit.FunSuite {
  import week4._

  test("zero") {
    val zero = Zero
    assert(zero.isZero)
  }

  test("one") {
    val one = Zero.successor
    assert(one - one == Zero)
  }
}
