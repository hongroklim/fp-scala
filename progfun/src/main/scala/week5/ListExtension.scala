package week5

object ListExtension {
  def removeAt[T](n: Int, xs: List[T]) = {
    def iter(i: Int, prev: List[T], next: List[T]): List[T] =
      if (next.isEmpty) prev
      else if (n == i) prev ++ next.tail
      else iter(i+1, prev :+ next.head, next.tail)

    iter(0, List(), xs)
  }

  def removeAt2[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop (n + 1))

  def flatten(xs: List[Any]): List[Any] = xs match {
    case Nil => List()
    case List(y :: ys1) :: ys2 => flatten(y :: (ys1 ::: ys2))
    //case y :: ys => y :: flatten(ys)
  }
}
