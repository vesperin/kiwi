package edu.ucsc.vesper.http.util

import edu.ucsc.refactor.util.SourceFormatter
import edu.ucsc.vesper.http.domain.Models.Code
import spray.http.DateTime

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

  def renderStatusPage() = {
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

    val codeId: String    = theCode.id.getOrElse("vesperized")
    val birthdate: String = DateTime(theCode.birthday.getOrElse(System.currentTimeMillis)).toIsoDateString

    val topFiveRelatedCode: List[Code] = if(relatedWork.size < 6) relatedWork.filter(x => x.id.getOrElse("vesperized") != codeId) else randomSelect(5, relatedWork).filter(x => x.id.getOrElse("vesperized") != codeId)


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
                    a(style:="color: black;", target:="_blank", href:= ("""http://www.vesperin.com/kiwi/render?q=id:""" + c.id.getOrElse(codeId)))(
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
