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

  def renderCodesnippet(theCode: Code) = {
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


    val likeButtonSettings: String =
      """<span class="likebtn-wrapper" """ +
        "data-identifier=\"" + codeId + "\"" +
        """ data-popup_enabled="false" data-show_dislike_label="true" data-popup_position="bottom"
          | data-item_url="http://www.vesperin.com/"
          | data-share_enabled="false"></span>
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
            |
            | .main_like{margin-bottom:10px; text-align:center;}
            |}
          """.stripMargin)
      ),

      body( style:= "padding: 0 2rem; color: rgb(20%,20%,20%); background: rgb(255, 255, 255); font-family: Courier, monospace;font-size: 1.6rem;",
        // main
        div(`class`:="banner well", style:="background: #ffffff; border: none")(
          p(`class`:= "main_like")(raw(likeDislikeButtons)),
          h3(theCode.name + ": ", a(style:="color: black;", href:= theCode.url.getOrElse(hrefUrl))(txtUrl)),
          p(raw(linkified)),
          // code
          div(
            pre(`class`:="line-numbers", style:= "border: 1px dashed #ddd; bottom:5px; background: rgb(90%,90%,90%); font-size: 1.6rem;") (
              code(id:=codeId, `class`:="language-clike",
                style:="display: block; font-size: 1.4rem; padding: 3px 4px 13px 4px; top: 0; background: rgb(90%,90%,90%);")(
                raw(new SourceFormatter().format(theCode.content))
              )
            )
          ), // end of code

          // comments
          div(
            h4(style:="border-bottom: 1px solid #e5e5e5;margin-top: 22px;")(strong(blockquotes.size), if(blockquotes.size > 1 ) " Comments" else " Comment"),
            div(
              blockquotes.map { x => blockquote(style:= "font-size: 1.6rem;")(x)}
            )
          ), // end of comments

          // footer
          div(id:="footer_wrap", `class`:= "outer")(
            footer(`class`:= "inner")(
              p(id:= "project_copyright", `class`:= "copyright")(
                "HTML was generated by ",
                strong()("Vesper"),
                ", a tool maintained by ",
                a(style:="color: black;", href:="https://github.com/hsanchez")("Huascar A. Sanchez")
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
