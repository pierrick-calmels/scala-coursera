package week6

/**
  * Created by pk on 31/07/17.
  */
object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if(k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k-1)
          col <- 0 until n
          if isSafe(col, queens)

        } yield col :: queens


    }
    //    my solution - wrong, see worksheet
    def isSafe(col: Int, ints: List[Int]): Boolean = {
      !ints.contains(col) &&
        (for {
          (j,k) <- ints.zip(1 to ints.size)
        } yield (j != col + k) && (j != col-k))
          .forall(_ => true)
    }
    //    def isSafe(col: Int, queens: List[Int]): Boolean = {
    //      val row = queens.length
    //      val queensWithRow = (row-1 to 0 by -1) zip queens
    //      queensWithRow forall {
    //        case (r, c) => col != c && math.abs(col-c) != row - r
    //      }
    //    }
    placeQueens(n)
  }
}
