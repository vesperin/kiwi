package edu.ucsc.vesper.http.util

import edu.ucsc.refactor.util.SourceFormatter
import edu.ucsc.vesper.http.domain.Models.Code

import scalatags.Text.all._
import scalatags.Text.tags2.title

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
object Html {

  private def linkify(text: String) = {
    LinkifyText.createLinks(text)
  }

  def ohSnap() = {
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

  def renderHelp() = {
    val help = html(style:= "font-size: 62.5%; -webkit-font-smoothing: antialiased; font-smoothing: antialiased;")(
      head(
        meta(charset := "utf-8"),
        title("Kiwi Vesper's Help"),
        link(rel:= "stylesheet", href:="//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"),
        link(rel:= "shortcut icon", href:="https://raw.githubusercontent.com/chefedited/chefedited.github.io/master/public/favicon2.ico")
      ),
      body( style:= "padding: 0 2rem; color: rgb(20%,20%,20%); background: rgb(255, 255, 255); font-family: Courier, monospace;font-size: 1.6rem;",
        div(`class`:="banner well", style:="background: #ffffff; border: none; max-width: 66rem; min-width: 66rem; margin:auto;")(
          h3("Hello, guest!"),
          p("I'm ", strong("Kiwi Vesper"), ", a close companion of Vesper, a code transformation library for Java source code. Kiwi exposes Vesper's API as RESTful service."),
          hr(style:= "border:none; display:hidden; margin: 2rem 0;clear:both;"),
          p("It was designed and developed by ", a(style:="color: black;", href:= "http://www.huascarsanchez.com")("Huascar Sanchez"), " as part of his research work on Source Code Curation."),
          hr(style:= "border:none; display:hidden; margin: 2rem 0;clear:both;"),
          p("You can report any experienced bugs at ", a(style:="color: black;", href:= "https://github.com/chefedited/vesper-http")("vesper-http"), "."),
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

  def renderCodesnippet(theCode: Code, relatedWork: List[Code], survey: Boolean) = {
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

    val codeId: String = theCode.id.getOrElse("vesperized")

    val topFiveRelatedCode: List[Code] = if(relatedWork.size < 6) relatedWork else randomSelect(5, relatedWork)


    val likeButtonSettings: String =
      """<span class="likebtn-wrapper" """ +
        "data-identifier=\"" + codeId + "\"" +
        """  data-show_dislike_label="true"
          |  data-counter_show="false"
          |  data-popup_enabled="false"
          |  data-popup_position="bottom"
          |  data-i18n_like="Yes" data-i18n_dislike="No"
          |  data-i18n_after_like="Yes" data-i18n_after_dislike="No"
          |  data-i18n_like_tooltip="Found it useful"
          |  data-i18n_dislike_tooltip="Found it useless"
          |  data-i18n_unlike_tooltip="Found it useful"
          |  data-i18n_undislike_tooltip="Found it useless"></span>
          | <script type="text/javascript" src="//w.likebtn.com/js/w/widget.js" async="async"></script>
        """.stripMargin

    val likeDislikeButtons: String = likeButtonSettings

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
            |.main_like{margin-bottom:10px; text-align:left;}
            |
            |
          """.stripMargin)
      ),

      body( style:= "padding: 0 2rem; color: rgb(20%,20%,20%); background: rgb(255, 255, 255); font-family: Courier, monospace;font-size: 1.5rem;",
        // main
        div(`class`:="banner well", style:="background: #ffffff; border: none")(
          h3(strong(theCode.name)),
          p(style:="margin-top:-1rem;", small("From ", a(style:="color: black;", href:= theCode.url.getOrElse(hrefUrl))(txtUrl))),
          p(style:="font-size: 1.6rem;", raw(linkified)),
          // code
          div(
            pre(`class`:="line-numbers", style:= "border: 1px dashed #ddd; bottom:5px; background: rgb(90%,90%,90%); font-size: 1.6rem;") (
              code(id:=codeId, `class`:="language-clike",
                style:="display: block; font-size: 1.4rem; padding: 3px 4px 13px 4px; top: 0; background: rgb(90%,90%,90%);")(
                raw(new SourceFormatter().format(theCode.content))
              )
            ),
            h5(scalatags.Text.all.title:="Confidence for reuse", "The level of confidence on whether this code can be reused is ", printStars(theCode.confidence)),
            if(survey) {
              div(
                p("Help us improve ", strong("vesper"), " by answering this question:"),
                h4("Did you find this code snippet useful?"),
                p(`class`:= "main_like")(raw(likeDislikeButtons))
              )
            } else {
              p()
            }
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
              p("The number of ★ implies how confident the curator is on whether this code snippet can be reused. For example, 1 ★ means ",
                span(style:="font-style:italic", "no confident;"), " 5 ★ means ",
                span(style:="font-style:italic", "highly confident"), "."),
              ol(
                if(topFiveRelatedCode.isEmpty)
                  li("None")
                else
                  topFiveRelatedCode.map(c => li(
                    a(style:="color: black;", target:="_blank", href:= ("""http://www.cookandstuff.com/kiwi/render?q=id:""" + c.id.getOrElse(codeId)))(
                      c.name + " "   + printStars(c.confidence)
                    )
                   )
                  )
              )
            )
          ), // end of comments

//          if(survey) {
//            div(
//              h5(style:="border-bottom: 1px solid #e5e5e5;margin-top: 22px;", "Survey"),
//              p("Help us improve ", strong("vesper"), " by answering this question:"),
//              h4("Did you find this code snippet useful?"),
//              p(`class`:= "main_like")(raw(likeDislikeButtons))
//            )
//          } else {
//            p()
//          }
//          ,

          hr(),
          // footer
          div(id:="footer_wrap", `class`:= "outer")(
            footer(`class`:= "inner")(
              p(id:= "project_copyright", `class`:= "copyright")(
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
                    src:= "https://ga-beacon.appspot.com/UA-52736669-3/kiwi/render/" + codeId + "?pixel&dt=" + theCode.name.stripSuffix(".java").toLowerCase,
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
          """.stripMargin)
      )
    )

    scala.xml.Unparsed(codesnippet.toString())
  }
}
