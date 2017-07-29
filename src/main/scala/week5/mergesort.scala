package week5

/**
  * Created by pk on 21/07/17.
  */
object mergesort {
  def mSort(x: List[Int]): List[Int] = {
    val n = x.length/2
    if(n == 0) x
    else {
        def merge(xs: List[Int],ys: List[Int]): List[Int] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (z :: zs, q :: qs) =>
            if(z < q) z :: merge(zs, ys)
            else q :: merge(xs, qs)
        }
      val (fst, snd) = x splitAt n
      merge(mSort(fst), mSort(snd))
    }
  }

  def generalmSort[T](x: List[T])(lt: (T,T) => Boolean): List[T] = {
    val n = x.length/2
    if(n == 0) x
    else {
      def merge(xs: List[T],ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (z :: zs, q :: qs) =>
          if(lt(z, q)) z :: merge(zs, ys)
          else q :: merge(xs, qs)
      }
      val (fst, snd) = x splitAt n
      merge(generalmSort(fst)(lt), generalmSort(snd)(lt))
    }
  }

  //msort equivalent
  def msort2(x:List[Int]): List[Int] = generalmSort(x)((x1: Int, y1: Int) => x1 < y1)
  def stringmSort(x: List[String]): List[String] = generalmSort(x)((x1:String , y1: String) => x1.compareTo(y1) < 0 )

  import scala.math.Ordering
  def generalmSort2[T](x: List[T])(ord: Ordering[T]): List[T] = {
    val n = x.length/2
    if(n == 0) x
    else {
      def merge(xs: List[T],ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (z :: zs, q :: qs) =>
          if(ord.lt(z, q)) z :: merge(zs, ys)
          else q :: merge(xs, qs)
      }
      val (fst, snd) = x splitAt n
      merge(generalmSort2(fst)(ord), generalmSort2(snd)(ord))
    }
  }
  val nums = List(-4,-2,100,8)
  val fruits = List("banana", "apple", "pineapple", "orange")
  generalmSort2(nums)(Ordering.Int)
  generalmSort2(fruits)(Ordering.String)


  //with implicit parameter, no need to pass the ordering anymore
  def generalmSort3[T](x: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = x.length/2
    if(n == 0) x
    else {
      def merge(xs: List[T],ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (z :: zs, q :: qs) =>
          if(ord.lt(z, q)) z :: merge(zs, ys)
          else q :: merge(xs, qs)
      }
      val (fst, snd) = x splitAt n
      merge(generalmSort3(fst), generalmSort3(snd))
    }
  }
  // see here
  generalmSort3(nums)
  generalmSort3(fruits)


}
