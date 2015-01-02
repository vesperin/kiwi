package com.vesperin.kiwi.spi

import com.vesperin.kiwi.domain.{Body, LongURL}
import org.scribe.builder.ServiceBuilder
import org.scribe.builder.api.GoogleApi
import org.scribe.model.{OAuthRequest, Response, Verb}
import org.scribe.oauth.OAuthService
import spray.json._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class UrlShortener {
  implicit def executionContext: ExecutionContextExecutor = ExecutionContext.Implicits.global

  // thx to http://codeoftheday.blogspot.com/2013/07/quick-and-easy-integration-of-google.html
  var oAuthService: OAuthService  = new ServiceBuilder().provider(classOf[GoogleApi])
    .apiKey("anonymous")
    .apiSecret("anonymous")
    .scope("https://www.googleapis.com/auth/urlshortener")
    .build()


  val oAuthRequest: OAuthRequest  = new OAuthRequest(Verb.POST, "https://www.googleapis.com/urlshortener/v1/url")
  oAuthRequest.addHeader("Access-Control-Allow-Origin", "*")
  oAuthRequest.addHeader("Content-Type", "application/json")

  def shortenUrl(url: String): Future[String] = {
    for {
      json    <- makeJsonString(url)
      resp    <- sendRequest(json)
      tinyUrl <- getShortUrl(resp)
    } yield {
      tinyUrl
    }
  }

  private[this] def makeJsonString(url: String): Future[String] =
    Future(LongURL(url).toJson.prettyPrint)

  private[this] def sendRequest(json: String): Future[Response] = {
    Future {
      oAuthRequest.addPayload(json)
      oAuthRequest.send()
    }
  }

  private[this] def getShortUrl(response: Response): Future[String] = {
    Future {
      val body: String      = response.getBody
      val jsonAst           = JsonParser(body)
      val bodyObject: Body  = jsonAst.convertTo[Body]
      bodyObject.id
    }
  }
}
