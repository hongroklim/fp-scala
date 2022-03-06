package clazz

import clazz.Rational

class RationalSuite extends munit.FunSuite {

  test("add") {
    val x = new Rational(1, 2)
    val y = new Rational(2, 3)
    assertEquals(x.add(y).toString, "7 / 6")
  }

  test("neg") {
    val x = new Rational(1, 2)
    assertEquals(x.neg.toString, "-1 / 2")
  }

  test("sub") {
    val x = new Rational(1, 2)
    val y = new Rational(1, 3)
    assertEquals(x.sub(y).toString, "1 / 6")
  }

  test("mul") {
    val x = new Rational(2, 3)
    val y = new Rational(5, 7)
    assertEquals(x.mul(y).toString, "10 / 21")
  }

  test("div") {
    val x = new Rational(4, 3)
    val y = new Rational(2, 3)
    assertEquals(x.div(y).toString, "12 / 6")
  }
}
