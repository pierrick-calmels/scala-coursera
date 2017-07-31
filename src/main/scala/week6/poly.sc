import Polynomials2.Poly

object Polynomials {
  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
      //      terms get exp match {
      //        case Some(coeff1) => exp -> (coeff + coeff1)
      //        case None => exp -> coeff
      //      }
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }
  val p1 = new Poly(1 -> 2.0, 3 -> 4, 5-> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
}


// more efficient because the ++ creates another list and then concatenates
object Polynomials2 {
  class Poly(val terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }
  val p1 = new Poly(1 -> 2.0, 3 -> 4, 5-> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
}
Polynomials2.p1
Polynomials2.p2
Polynomials2.p1 + Polynomials2.p2
Polynomials.p1
Polynomials.p2
Polynomials.p1 + Polynomials.p2
