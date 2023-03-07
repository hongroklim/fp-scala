package example

class ComparatorSuite extends munit.FunSuite {
  test("Purchase a good") {
    val goods: List[ComparableGood] = List(new ComparableGood("a", 10), new ComparableGood ("b", 10))
    assert(CustomBuyer.purchase(goods) == goods.head)

    implicit val ord: Ordering[ComparableGood] = Ordering.by(_.price)
    assert(goods.min == goods.head)

    println(goods)
  }
}
