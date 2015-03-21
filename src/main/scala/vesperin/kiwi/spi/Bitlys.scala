package vesperin.kiwi.spi

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class Bitlys(apiKey1: String, apiKey2: String) {
  import com.rosaloves.bitlyj.Bitly

  lazy val bitly = Bitly.as(apiKey1, apiKey2)

  def shorten(s: String): Option[String] = {
    try {
      val url:String = bitly.call(Bitly.shorten(s)).getShortUrl
      Option(url)
    } catch {
      case e: Exception =>
        None
    }
  }

  // try, or else return original
  def tryShorten(s: String): String = shorten(s) getOrElse s

}
