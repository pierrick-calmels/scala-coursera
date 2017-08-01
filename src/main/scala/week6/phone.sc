
object x {
  import scala.io.Source

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI",
    '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS",
    '8' -> "TUV", '9' -> "WXYZ"
  )

  /** Invert the mnem map **/
  val charCode: Map[Char, Char] =
    for((digit, str) <- mnem; ltr <- str) yield ltr -> digit

  /** Map word to digit**/
  def wordCode(word: String): String = word.toUpperCase map charCode

  wordCode("scalaisfun")

  object H {
    val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt").getLines
    val words = in.toList filter (_ forall (_.isLetter))
    /** Map digits code to a list of words **/
    val wordsForNum: Map[String, Seq[String]] =
      words groupBy wordCode withDefaultValue List()
  }
  import H._

  /** Return all ways to encode a number as a list of words **/
  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        first <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield first :: rest
    }.toSet
  }

  encode("7225247386")


  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")

  translate("7225247386")

}
