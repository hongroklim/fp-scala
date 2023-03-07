package example

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

case class Good(val name: String, val price: Int) extends Ordered[Good] {
  def compare(that: Good): Int = this.price.compare(that.price)
  override def toString(): String = s"$name ($price)"
}

trait Buyer {
  /* Return true when all the prices are same. */
  def areEquals(goods: List[Good]): Boolean

  /* Get a good which is the lowest price. */
  def cheapest(goods: List[Good]): Good

  final def purchase(goods: List[Good]): Good = {
    if (goods.isEmpty) throw new UnsupportedOperationException("empty.purchase")
    else if (areEquals(goods)) cheapest(goods)
    else goods.head
  }
}

object FunctionalBuyer extends Buyer {
  def areEquals(goods: List[Good]): Boolean = {
    def iter(aPrice: Int, goods: List[Good]): Boolean = {
      if (goods.isEmpty) true
      else if (aPrice != goods.head.price) false
      else iter(aPrice, goods.tail)
    }

    iter(goods.head.price, goods.tail)
  }
  
  def cheapest(goods: List[Good]): Good = {
    def iter(min: Good, goods: List[Good]): Good = {
      if (goods.isEmpty) min
      else {
        val lower = if(min.price > goods.head.price) goods.head else min
        iter(lower, goods.tail)
      }
    }

    iter(goods.head, goods.tail)
  }
}

object ListBuyer extends Buyer {
  def areEquals(goods: List[Good]): Boolean =
    goods.tail.count(_.compare(goods.head) != 0) == 0

  def cheapest(goods: List[Good]): Good = goods.min
}

object ParallelBuyer extends Buyer {
  def areEquals(goods: List[Good]): Boolean =
    goods.tail.par.forall(_.compare(goods.head) == 0)

  def cheapest(goods: List[Good]): Good = {
    def getMin(x: Good, y: Good): Good = if (x.compare(y) > 0) x else y
    goods.par.reduce(getMin)
  }
}
