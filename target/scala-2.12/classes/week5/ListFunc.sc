def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}


// O(|xs|)
def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

// xs ::: ys -> we match xs because the first element of the result
// depends on the first element of xs
// O(|xs|)
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

// the recursive call is O(|xs|)
// the reverse function is O(|xs|²)
def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  // ++ is O(|ys|)
  case y :: ys => reverse(ys) ++ List(y)
}

val l = List(1,1,2,3,5)
init(l)
last(l)

def removeAt[T](xs: List[T], n: Int): List[T] = xs match {
  case List() => throw new Error("remove on empty list")
//  case List(x) => if(n == 0) List() else throw new Error("remove on empty list")
  case y :: ys => if(n == 0) ys else y :: removeAt(ys, n-1)
}

def betterRemoveAt[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop n+1)


def squareList(xs: List[Int]): List[Int] = xs map( x => x*x)
def patternsql(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y*y :: patternsql(ys)
}

def pack[T](xs: List[T]): List[List[T]] = xs match  {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span(y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T,Int)] = {
  val packed = pack(xs)
  packed.map(y => (y.head, y.length))
}

pack(l)
encode(l)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( ??? )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( ??? )