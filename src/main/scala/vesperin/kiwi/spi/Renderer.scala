package vesperin.kiwi.spi

import vesperin.kiwi.db.{MongoStorage, Storage}
import vesperin.kiwi.domain.{Code, ExactlyOne, Result}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.xml.Unparsed

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Renderer {
  implicit def executionContext: ExecutionContextExecutor = ExecutionContext.Implicits.global

  val flattener: Flattener = ResultFlattener()


  private[this] def flatResult(optionResult: Option[Result]): Future[List[Code]] = optionResult match {
    case Some(r)  =>

      Future {
        val answer: List[Code] = flattener.flatten(r) match {
          case Nil    => List()
          case x::xs  => (x :: xs).asInstanceOf[List[Code]]
        }

        answer
      }

    case None    => Future (List())

  }


  private[this] def collectRelatedWork(theCode: Option[Code], storage: Storage): Future[List[Code]] = theCode match {
    case Some(c) =>

      for {
        result        <- storage.find(ExactlyOne("url", c.url.getOrElse("http://www.chefedited.com")))
        relatedWork   <- flatResult(result)
      } yield {
        relatedWork
      }

    case None    =>  Future(List())
  }


  private[this] def findSingleCode(list: List[Code]): Future[Option[Code]] = {
    Future {
      list match {
        case x :: xs  => Some(x)
        case _        => None
      }
    }
  }


  private[this] def buildHtml(theCode: Option[Code], relatedWork: List[Code], survey: Boolean): Future[Unparsed] = theCode match {
    case Some(c) =>
      Future(
        Html.buildCodesnippetHtml(
          c,
          relatedWork,
          survey
        )
      )
    case None    =>
      Future(
        Html.buildErrorPage()
      )
  }


  private[this] def buildIndexHtml(): Future[Unparsed] = Future(Html.buildStatusPage())

  private[this] def buildErrorHtml(): Future[Unparsed] = Future(Html.buildErrorPage())


  private[this] def collectCode(res: Option[Result]): Future[Option[Code]] = {
    for {
      codeList <- flatResult(res)
      theCode  <- findSingleCode(codeList)
    } yield {
      theCode
    }
  }

  def renderHtml(optionResult: Option[Result], survey: Boolean): Future [Unparsed] = {
    for {
      theCode     <- collectCode(optionResult)
      storage     <- Future(MongoStorage())
      relatedWork <- collectRelatedWork(theCode, storage)
      unparsed    <- buildHtml(theCode, relatedWork, survey)
    } yield {
      unparsed
    }
  }

  def renderStatusHtml(): Future[Unparsed] = {
    for{
      page <- buildIndexHtml()
    } yield {
      page
    }
  }

  def renderError(): Future[Unparsed] = {
    for{
      page <- buildErrorHtml()
    } yield {
      page
    }
  }
}

object Linkify {
  import java.util.regex.Pattern

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

  def createLinks(text: String) = Linkify.replaceHashtagCode(
    Linkify.replaceAtCode(
      Linkify.wrapUserLinks(
        Linkify.wrapUserLists(
          Linkify.wrapHashtags(
            Linkify.wrapLinks(text)
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

  private def wrapUserLinks(text: String) = text.replaceAll(Linkify.usernameRegex,
    "<a style='color: black;' href='" + Linkify.usernameUrl + "' target='_blank'>" + atCode + "$1</a>")

  private def wrapLinks(text: String): String = {
    val items = getLinkItems(text) // Example: ("url", "url")
    var newText = text
    items.foreach(item => {
      newText = newText.replace(item.title, "<a href='" + item.title + "'>" + item.title + "</a>")
    })
    newText
  }

  private def wrapHashtags(text: String) = text.replaceAll(Linkify.hashtagsRegex,
    "<a style='color: black;' href='" + Linkify.hashtagUrl + "%20%40codedetour' target='_blank'>" + hashtagCode + "$1</a>")

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

  private val trailingPunctuationRe = "^(.*?)[,.\"”]?$".r

  private def stripTrailingPunctuation(text: String) = text match {
    case trailingPunctuationRe(result) => result
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

object Html {
  import edu.ucsc.refactor.util.SourceFormatter
  import spray.http.DateTime

  import scalatags.Text.all._
  import scalatags.Text.tags2.title

  private def linkify(text: String) = {
    Linkify.createLinks(text)
  }

  def buildErrorPage() = {
    val help = html(style:= "font-size: 62.5%; -webkit-font-smoothing: antialiased; font-smoothing: antialiased;")(
      head(
        meta(charset := "utf-8"),
        title("Kiwi Vesper's Help"),
        link(rel:= "stylesheet", href:="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"),
        link(rel:= "shortcut icon", href:="https://raw.githubusercontent.com/chefedited/chefedited.github.io/master/public/favicon2.ico")
      ),
      body( style:= "padding: 0 2rem; color: rgb(20%,20%,20%); background: rgb(255, 255, 255); font-family: Courier, monospace;font-size: 1.6rem;",
        div(`class`:="banner well", style:="background: #ffffff; border: none; max-width: 66rem; min-width: 66rem; margin:auto;")(
          h3(strong("Oh, Snap!")),
          p("There is nothing to render.  "),
          hr(style:= "border:none; display:hidden; margin: 2rem 0;clear:both;"),
          p("If you believe this is a mistake, please report it as an issue at ", a(style:="color: black;", href:= "https://github.com/chefedited/vesper-http")("vesper-http"), "."),
          hr(style:= "border:none; display:hidden; margin: 2rem 0;clear:both;"),
          div(id:="footer_wrap", `class`:= "outer")(
            footer(`class`:= "inner")(
              p(id:= "project_copyright", `class`:= "copyright")(
                "© 2014 Huascar Sanchez."
              )
            )
          )
        ),
        script(src:="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
      )
    )

    scala.xml.Unparsed(help.toString())
  }

  def buildStatusPage() = {
    val help = html(style:= "font-size: 62.5%; -webkit-font-smoothing: antialiased; font-smoothing: antialiased;")(
      head(
        meta(charset := "utf-8"),
        title("Kiwi Vesper's Help"),
        link(rel:= "stylesheet", href:="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"),
        link(rel:= "shortcut icon", href:="https://raw.githubusercontent.com/chefedited/chefedited.github.io/master/public/favicon2.ico")
      ),
      body( style:= "padding: 0 2rem; color: rgb(20%,20%,20%); background: rgb(255, 255, 255); font-family: Courier, monospace;font-size: 1.6rem;",
        div(`class`:="banner well", style:="background: #ffffff; border: none; max-width: 66rem; min-width: 66rem; margin:auto;")(
          h3(span(style:="color:#898989;", "Status:"), span(style:="font-size: 2.2rem;font-style:normal;",  " Kiwi service is up and running!")),
          p("Need help on how to use it? If you do, then ", a(style:= "color:black;", href:= "http://www.vesperin.com/kiwi/help")("Click here.")),
          div(id:="footer_wrap", `class`:= "outer")(
            footer(`class`:= "inner")(
              p(id:= "project_copyright", `class`:= "copyright")(
                "© 2014 Huascar Sanchez."
              )
            )
          )
        ),
        script(src:="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
      )
    )

    scala.xml.Unparsed(help.toString())
  }

  // thx to http://aperiodic.net/phil/scala/s-99/
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (preElement, e :: postElement)  => (preElement ::: postElement, e)
    case (preElement, Nil)               => throw new NoSuchElementException
  }


  // It can be expensive to create a new Random instance every time, so let's
  // only do it once. thx to http://aperiodic.net/phil/scala/s-99/
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }

  private def printStars(counter: Int): String = {
    val builder = new StringBuilder
    for( a <- 1 to counter){
      builder.append("★")
    }

    builder.toString()
  }

  def buildCodesnippetHtml(theCode: Code, relatedWork: List[Code], survey: Boolean) = {
    val hrefUrl: String   = theCode.url.getOrElse("#")
    var txtUrl:String     = theCode.url.getOrElse("(Missing URL)")
    txtUrl = txtUrl.stripPrefix("http://")

    val linkified = linkify(theCode.description.stripSuffix(".") + " " +
      List.concat(
        theCode.tags,
        theCode.algorithms,
        theCode.datastructures
      ).distinct.map(x => "#" + x).mkString(" "))

    val blockquotes = scala.collection.mutable.ListBuffer.empty[String]
    for(comment <- theCode.comments){
      blockquotes +=  ("["
        + (comment.from.split(";")(0).toInt + 1) + "-"
        +  (comment.to.split(";")(0).toInt + 1) + "]: "
        + comment.text
        + ".")
    }

    def getIdOrElse(text: String): String = if(text != null && !text.isEmpty) text else "vesperized"

    val codeId: String    = getIdOrElse(theCode.id)
    val birthdate: String = DateTime(theCode.birthday.getOrElse(System.currentTimeMillis)).toIsoDateString

    val topFiveRelatedCode: List[Code] = if(relatedWork.size < 6) relatedWork.filter(x => getIdOrElse(x.id) != codeId) else randomSelect(5, relatedWork).filter(x => getIdOrElse(x.id) != codeId)


    val codesnippet = html(
      style:= "font-size: 62.5%; -webkit-font-smoothing: antialiased; font-smoothing: antialiased;",
      head(
        meta(charset := "utf-8"),
        title(theCode.name),

        /* css links */
        link(rel:= "stylesheet", href:="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"),
        link(rel:= "stylesheet", href:="https://raw.githubusercontent.com/LeaVerou/prism/gh-pages/themes/prism.css"),
        link(rel:= "stylesheet", href:="http://prismjs.com/plugins/line-numbers/prism-line-numbers.css"),
        link(rel:= "shortcut icon", href:="https://raw.githubusercontent.com/chefedited/chefedited.github.io/master/public/favicon2.ico"),
        scalatags.Text.tags2.style(
          """
            |.token.operator,
            |.token.entity,
            |.token.url,
            |.language-css .token.string,
            |.style .token.string,
            |.token.variable {
            |	color: black;
            |	background: rgb(90%,90%,90%);
            |}
            |
            |.container {
            |	max-width: 92rem;
            |	min-width: 92rem;
            |	margin:auto;
            |}
            |
            |.btn-inverse {
            |    color: #ffffff;
            |    background-color: #34495e;
            |}
            |.btn-inverse:hover,
            |.btn-inverse:focus,
            |.btn-inverse:active,
            |.btn-inverse.active,
            |.open .dropdown-toggle.btn-inverse {
            |    color: #ffffff;
            |    background-color: #415b76;
            |    border-color: #415b76;
            |}
            |.btn-inverse:active,
            |.btn-inverse.active,
            |.open .dropdown-toggle.btn-inverse {
            |    background: #2c3e50;
            |    border-color: #2c3e50;
            |}
            |.btn-inverse.disabled,
            |.btn-inverse[disabled],
            |fieldset[disabled] .btn-inverse,
            |.btn-inverse.disabled:hover,
            |.btn-inverse[disabled]:hover,
            |fieldset[disabled] .btn-inverse:hover,
            |.btn-inverse.disabled:focus,
            |.btn-inverse[disabled]:focus,
            |fieldset[disabled] .btn-inverse:focus,
            |.btn-inverse.disabled:active,
            |.btn-inverse[disabled]:active,
            |fieldset[disabled] .btn-inverse:active,
            |.btn-inverse.disabled.active,
            |.btn-inverse[disabled].active,
            |fieldset[disabled] .btn-inverse.active {
            |    /*background-color: #34495e;*/
            |    /*border-color: #34495e;*/
            |    background-color: #d6dbdf;
            |    border-color:#d6dbdf;
            |}
            |
          """.stripMargin)
      ),

      body( style:= "padding: 0 2rem; color: rgb(20%,20%,20%); background: rgb(255, 255, 255); font-family: Courier, monospace;font-size: 1.5rem;",
        // main
        div(`class`:="container", style:="background: #ffffff; border: none")(
          h3(strong(theCode.name)),
          p(style:="margin-top:-1rem;", small(style:="font-size: 1.2rem;", "From ", a(style:="color: black;", href:= theCode.url.getOrElse(hrefUrl))(txtUrl))),
          p(style:="font-size: 1.6rem;", raw(linkified)),
          // code
          div(
            pre(`class`:="line-numbers", style:= "border: 1px dashed #ddd; bottom:5px; background: rgb(90%,90%,90%); font-size: 1.3rem;") (
              code(id:=codeId, `class`:="language-clike",
                style:="display: block; font-size: 1.4rem; padding: 3px 4px 13px 4px; top: 0; background: rgb(90%,90%,90%);")(
                  raw(new SourceFormatter().format(theCode.content))
                )
            ),

            h5(scalatags.Text.all.title:="Confidence for reuse", "The level of confidence on whether this code can be reused is ", printStars(theCode.confidence))
          ), // end of code

          // comments
          div(
            h4(style:="border-bottom: 1px solid #e5e5e5;margin-top: 22px;")(strong(blockquotes.size), if(blockquotes.size > 1 ) " Comments" else " Comment"),
            div(
              blockquotes.map { x => blockquote(style:= "font-size: 1.6rem;")(x)}
            )
          ), // end of comments

          div(
            h4(style:="border-bottom: 1px solid #e5e5e5;margin-top: 22px;")("Related work ", "(", strong(topFiveRelatedCode.size), ")"),
            div(
              p(style:="font-size: 1.6rem;", "The number of ★ implies how confident the curator is on whether this code snippet can be reused. For example, 1 ★ means ",
                span(style:="font-style:italic", "no confident;"), " 5 ★ means ",
                span(style:="font-style:italic", "highly confident"), "."),
              ol(
                if(topFiveRelatedCode.isEmpty)
                  li(style:="font-size: 1.6rem;", "None")
                else
                  topFiveRelatedCode.map(c => li(style:="font-size: 1.6rem;",
                    a(style:="color: black;", target:="_blank", href:= ("""http://www.vesperin.com/kiwi/render?q=id:""" + getIdOrElse(c.id)))(
                      String.format("%-32s %s", c.name, printStars(c.confidence)) + " ", span(style:="font-size:12px; font-style:italic; color: #999;", birthdate)
                    )
                  )
                  )
              )
            )
          ), // end of related work

          if(survey) {
            div(
              h4(style:="border-bottom: 1px solid #e5e5e5;margin-top: 22px;", "Survey"),
              p(style:="font-size: 1.6rem;", "Help us improve ", strong("vesper"), " by taking the following survey:"),
              p(raw("""<a class="typeform-share btn btn-sm btn-inverse" href="http://goo.gl/vhg140" data-mode="1" target="_blank">Launch me!</a>"""))
            )
          } else {
            p()
          }
          ,

          // survey

          hr(),
          // footer
          div(id:="footer_wrap", `class`:= "outer")(
            footer(`class`:= "inner")(
              p(style:="font-size: 1.6rem;", id:= "project_copyright", `class`:= "copyright")(
                "HTML was generated by ",
                strong()("Vesper"),
                ", a tool maintained by ",
                a(style:="color: black;", href:="https://github.com/hsanchez")("Huascar A. Sanchez")
              ),

              // tracking each page with google analytics
              // thx to
              // https://github.com/igrigorik/ga-beacon
              // http://www.sitepoint.com/using-beacon-image-github-website-email-analytics/
              p(
                a(href:="https://github.com/igrigorik/ga-beacon")(
                  img(
                    // Production server: UA-52905425-1
                    // Staging server: UA-52736669-3
                    src:= "https://ga-beacon.appspot.com/UA-52905425-1/kiwi/render/" + codeId + "?pixel&dt=" + theCode.name.stripSuffix(".java").toLowerCase,
                    alt:= "Analytics", style:= "max-width:100%;"
                  )
                )
              )
            )
          ) // end of footer
        ), // end of main
        script(src:="https://code.jquery.com/jquery-2.1.1.js"),
        script(src:="http://prismjs.com/prism.js"),
        script(src:="http://prismjs.com/plugins/line-numbers/prism-line-numbers.js"),
        script(src:="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"),
        script(
          """
            |  function selectText(element){
            |    var range, selection;
            |    if (document.body.createTextRange) {
            |       range = document.body.createTextRange();
            |       range.moveToElementText(element);
            |       range.select();
            |    } else if (window.getSelection) {
            |     selection = window.getSelection();
            |     range = document.createRange();
            |     range.selectNodeContents(element);
            |     selection.removeAllRanges();
            |     selection.addRange(range);
            |   }
            |  }
            |
            |  (function($){
            |
            |  $('code').each(function(){
            |    var code = $(this);
            |    var flag;
            |    code.on('mousedown', function() {
            |        flag = 0;
            |    });
            |
            |    code.on('mousemove', function(){
            |        flag = 1;
            |    });
            |
            |    code.on('mouseup', function(){
            |        if(flag === 0){
            |            selectText(jQuery(this)[0]);
            |        }
            |    });
            |  });
            |
            |   }(window.jQuery))
            |
            |
            |
            |  (function () {
            |    var qs, js, q, s, d = document, gi = d.getElementById, ce = d.createElement, gt = d.getElementsByTagName, id = 'typef_orm', b = 'https://s3-eu-west-1.amazonaws.com/share.typeform.com/';
            |    if (!gi.call(d, id)) {
            |        js = ce.call(d, 'script');
            |        js.id = id;
            |        js.src = b + 'share.js';
            |        q = gt.call(d, 'script')[0];
            |        q.parentNode.insertBefore(js, q)
            |    }
            |    id = id + '_';
            |    if (!gi.call(d, id)) {
            |        qs = ce.call(d, 'link');
            |        qs.rel = 'stylesheet';
            |        qs.id = id;
            |        qs.href = b + 'share-button.css';
            |        s = gt.call(d, 'head')[0];
            |        s.appendChild(qs, s)
            |    }
            |  })()
            |
            |
          """.stripMargin)
      )
    )

    scala.xml.Unparsed(codesnippet.toString())
  }

}

case class HtmlRenderer() extends Renderer
