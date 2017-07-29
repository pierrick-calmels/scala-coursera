object test {

  val xs = Array(1, 2, 3, 44)
  xs map (x => x * 2)

  val s = "Hello World"
  s filter (c => c.isUpper)
  s exists (c => c.isUpper)
  s forall (c => c.isUpper)

  val pairs = List(1, 2, 3) zip s
  pairs.unzip
  s flatMap (c => List('.', c))

  def allCombinations(m: Int, n: Int) = {
    val r = 1 to m
    r flatMap (x => (1 to n) map (y => (x, y)))
  }

  allCombinations(4, 5)

  def scalarProduct(x: Vector[Int], y: Vector[Int]): Int =
    (x zip y).map(xy => xy._1 * xy._2).sum

  val v1: Vector[Int] = Vector(1,2,3)
  val v2: Vector[Int] = Vector(4,5,6)
  scalarProduct(v1,v2)

  def scalarProductPattern(x: Vector[Int], y: Vector[Int]): Int =
    (x zip y).map{case (xs, ys) => xs*ys}.sum
//    (x zip y).map{v => v match {case (xs, ys) => xs*ys}}.sum // same


  scalarProductPattern(v1,v2)

  // my version
  //  def isPrime(n: Int): Boolean = !((2 until n) exists (r => n % r == 0))
  def isPrime(n: Int): Boolean = (2 until n) forall (r => n % r != 0)

  isPrime(10)
  isPrime(11)
  isPrime(2)


}