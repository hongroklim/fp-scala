package week3

class IntSetSuite extends munit.FunSuite {
  import week3._

  test("empty set") {
    val e = Empty
    assert(!e.contains(1))
  }

  test("single set") {
    val e = Empty
    val s = e incl 1
    assert(s contains 1)
  }

  test("expand tree") {
    val t1 = Empty
    val t2 = t1 incl 5
    val t3 = t2 incl 3
    val t4 = t3 incl 7
    assert(t4 contains 3)
    assert(t4 contains 5)
    assert(t4 contains 7)
  }
}
