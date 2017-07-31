import scala.io.Source

object x {
  val in = Source.fromURL("http:/lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords")

  val words = in.getLines

  val mnem = Map(
    '2'
  )

}
