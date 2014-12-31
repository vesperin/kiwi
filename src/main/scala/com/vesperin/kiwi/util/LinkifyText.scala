package com.vesperin.kiwi.util

import java.util.regex.Pattern

/**
 * Extracts links from Twitter status; thx to https://github.com/dcbriccetti/talking-puffin
 */
object LinkifyText {

  val urlCharClass = """[^'"()\[\]\s]"""
  val hyperlinkRegex = "(https?://" + urlCharClass + "+)"
  val userListRegex = """@((\w+)/""" + urlCharClass + "+)"
  val usernameRegex = """@(\w+)"""
  val hashtagsRegex = """#(\w+)"""
  val usernameUrl = linkForAnalyze("$1")
  val hashtagUrl  = hashTagForAnalyze("$1")
  val hyperlinkPattern = Pattern.compile(hyperlinkRegex)
  val userListPattern = Pattern.compile(userListRegex)
  val usernamePattern = Pattern.compile(usernameRegex)

  case class Link(title: String, link: String)
  type Links = List[Link]


  def linkForAnalyze(user: String) = "http://twitter.com/" + user
  def hashTagForAnalyze(tag: String) = "https://twitter.com/search/?src=typd&q=%23" + tag

  def createLinks(text: String) = LinkifyText.replaceHashtagCode(
    LinkifyText.replaceAtCode(
      LinkifyText.wrapUserLinks(
        LinkifyText.wrapUserLists(
          LinkifyText.wrapHashtags(
            LinkifyText.wrapLinks(text)
          )
        )
      )
    )
  )


  val withoutUserPattern = Pattern.compile("""^@\S+ (.*)""")

  /**
   * Returns a string with any @user at the beginning removed.
   */
  def getWithoutUser(text: String): String = {
    val m = withoutUserPattern.matcher(text)
    if (m.find) m.group(1) else text
  }

  private val atCode = "##xX##"
  private val hashtagCode = "##yY##"

  private def wrapUserLinks(text: String) = text.replaceAll(LinkifyText.usernameRegex,
    "<a style='color: black;' href='" + LinkifyText.usernameUrl + "' target='_blank'>" + atCode + "$1</a>")

  private def wrapLinks(text: String): String = {
    val items = getLinkItems(text) // Example: ("url", "url")
    var newText = text
    items.foreach(item => {
      newText = newText.replace(item.title, "<a href='" + item.title + "'>" + item.title + "</a>")
    })
    newText
  }

  private def wrapHashtags(text: String) = text.replaceAll(LinkifyText.hashtagsRegex,
    "<a style='color: black;' href='" + LinkifyText.hashtagUrl + "%20%40codedetour' target='_blank'>" + hashtagCode + "$1</a>")

  private def wrapUserLists(text: String): String = {
    val items = getListItems(text) // Example: ("dave/scala", "http://twitter.com/dave/scala")
    var newText = text
    items.foreach(item => {
      newText = newText.replace("@" + item.title,
        "<a style='color: black;' href='" + item.link + "'>" + atCode + item.title + "</a>")
    })
    newText
  }

  private def replaceAtCode(text: String) = text.replaceAll(atCode, "@")
  private def replaceHashtagCode(text: String) = text.replaceAll(hashtagCode, "#")

  private val trailingPunctRe = "^(.*?)[,.\"”]?$".r

  private def stripTrailingPunctuation(text: String) = text match {
    case trailingPunctRe(result) => result
  }

  /**
   * Finds hyperlinks.
   */
  private def getLinkItems(text: String): Links = {
    var urls = List[Link]()
    val m = hyperlinkPattern.matcher(text)
    while (m.find) {
      val item = stripTrailingPunctuation(m.group(1))
      val newItem = Link(item, item)
      if (! urls.contains(newItem))
        urls = urls ::: List(newItem)
    }
    urls
  }

  /**
   * Finds lists, like @dcbriccetti/scala.
   */
  private def getListItems(text: String): Links = {
    var urls = List[Link]()
    val m = userListPattern.matcher(text)
    while (m.find) {
      val item = stripTrailingPunctuation(m.group(1))
      val newItem = Link(item, "http://twitter.com/" + item)
      if (! urls.contains(newItem))
        urls = urls ::: List(newItem)
    }
    urls
  }
}
