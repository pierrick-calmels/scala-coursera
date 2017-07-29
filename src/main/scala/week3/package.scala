/**
  * Created by pk on 11/07/17.
  */
package object week3 {

  import java.util.NoSuchElementException

  /**
    * Created by pk on 11/07/17.
    */
  trait List[T] {
    def isEmpty: Boolean

    def head: T

    def tail: List[T]

  }

  // val in the constructor makes a field,
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }

  class Nil[T] extends List[T] {
    def isEmpty = true

    def head: Nothing = throw new NoSuchElementException("Nil.head")

    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }

  //def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
}
