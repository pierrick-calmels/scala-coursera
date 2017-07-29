package week4

/**
  * Created by pk on 13/07/17.
  */
abstract class IntSet {
  def assertAllPos[S <: IntSet](set: S): S = ???
  // S <: T means S is a subtype of T
  // S >: T means S supertype of T
  // covariance
  /*
  if S <: T
  is List[S] <: List[T]?
  it causes issues. contrex: a: NonEmpty[] = [b]
  a.remove(b) -> a:NonEmpty[] = ["Empty"]
  ARRAYS ARE NOT COVARIANT IN SCALA
   */
  //  asInstanceOf and isInstanceOf are not recommended in scala
  // because it is low level and potentially unsafe
}

class NonEmpty extends IntSet
class Empty extends IntSet



trait Expr{
  // pattern matching
  // better if lots of functions with static implementation
  def eval1(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval1(e1) + eval1(e2)
    case Prod(e1, e2) => eval1(e1) * eval1(e2)
  }


  def eval2: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval2 + e2.eval2
    case Prod(e1, e2) => e1.eval2 * e2.eval2
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Prod(e1, Sum(e2, e3)) => e1.show + " * (" + e2.show + " + " + e3.show + ")"
    case Prod(Sum(e2, e3), e1) => "(" + e2.show + " + " + e3.show + ") * "+ e1.show
    case Var(name) => name
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Prod(e1, e2) => e1.show + " * " + e2.show
  }
}
// lots of objects/subclasses, OO approach better
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

object Main{
  def main(args: Array[String]): Unit = {
    val x1 = Sum(Prod(Number(2), Var("x")),Var("y"))
    val x2 = Prod(Sum(Number(2), Var("x")),Var("y"))
    println(x1.show)
    println(x2.show)
    println(Prod(x1, x2).show)

  }
}