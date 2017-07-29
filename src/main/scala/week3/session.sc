object nth {
  import week3._

  def nth[T](n: Int, list: List[T]): T =
    if (list.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) list.head
    else nth(n - 1, list.tail)


  val z = new Cons[Int](1, new Cons(2, new Cons(3, new Nil[Int])))

  nth(4, z)
  nth(2, z)
  nth(-1, z)

}
