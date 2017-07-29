package week4

/**
  * Created by pk on 13/07/17.
  */

// Lists - Immutable and recursive
// List constructor : List(x, y, z) == x :: y :: z = z.::(y).::(x)
// Arrays - Mutable and Flat
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

}

class Cons[T](val head: T,val tail:List[T]) extends List[T] {
  def isEmpty = false
}
class Nil[T] extends List[T]{
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, new Nil))
  def apply[T](x: T): List[T] = new Cons(x, new Nil)
  def apply[T]() = new Nil

  // O(N*N)
  def insertSort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, insertSort(ys))
  }
  // O(xs)
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if(x <= y) x :: xs else y :: insert(x, ys)
  }

}
