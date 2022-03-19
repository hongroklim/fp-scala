package week5

object HOListFunc {
  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys) 
  }

  def mapSquareList(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (fst, lst) = xs span (p => p == x)
      fst :: pack(lst)
    }
    // case x :: xs1 => (xs takeWhile ((p: T) => p == x)) :: pack(xs dropWhile ((p: T) => p == x))
  }

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (fst, lst) = xs span (p => p == x)
      (x, fst.length) :: encode(lst)
    }
  }

  def mapEncode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (x => (x.head, x.length))
}
