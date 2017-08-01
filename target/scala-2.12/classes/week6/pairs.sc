def isPrime(n: Int): Boolean = (2 until n) forall (r => n % r != 0)

def primePairs(n: Int) = {
  ((1 until n) flatMap (i =>
    (1 until i) map (j => (i,j)))).filter {
    case (i: Int,j: Int) => isPrime(i+j)
  }
}

primePairs(7)

def iterPrimePairs(n: Int) = {
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i+j)
  } yield (i, j)
}

primePairs(7) == iterPrimePairs(7)

def iterScalaProduct(xs: List[Double], ys: List[Double]) =
  (for((x,y) <- xs.zip(ys)) yield x*y).sum
}
iterScalaProduct(List(1,2), List(1,2))
