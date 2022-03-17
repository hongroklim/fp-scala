package week3

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def nth(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
  def nth(n: Int): T = {
    def loop(i: Int, xs: List[T]): T =
      if (xs.isEmpty) throw new IndexOutOfBoundsException
      else if (i == n) xs.head
      else loop(i + 1, xs.tail)

    loop(0, new Cons(head, tail))
  }
}

/*
 * In object, generic argument can't be used.
 * `Nothing` is the supertype.
 */
//class Nil[T] extends List[T] {
object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def nth(n: Int): Nothing = throw new IndexOutOfBoundsException("Nil.empty")
}

/*
 * List can be expressed by function
 */
object List {
  // List(1, 2) = [1, 2]
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))

  // List() = [] /* empty list */
  def apply[T]() = Nil
}
