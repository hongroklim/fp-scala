package week5

object Reduction {
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( (x, y) => f(x) :: y )

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (x, y) => 1 + y )
}
