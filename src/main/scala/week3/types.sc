import week3._

object scratch {
  def error(msg: String) = throw new Error(msg)

  val x = null
  val y: String = x
//  val z: Int = null -> usually not permitted
  if (true) 1 else false
  if (true) 1 else "damn" // primitive type and java.lang.String :D
  val z = Nil
}