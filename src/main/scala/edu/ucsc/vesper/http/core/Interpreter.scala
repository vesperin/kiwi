package edu.ucsc.vesper.http.core

import edu.ucsc.refactor._
import edu.ucsc.refactor.spi._
import edu.ucsc.vesper.http.config.Configuration
import edu.ucsc.vesper.http.domain.Models._
import edu.ucsc.vesper.http.util.{GoogleUrlShortener, Html}
import twitter4j.conf.ConfigurationBuilder
import twitter4j.{Status, Twitter, TwitterFactory}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.{ExecutionContextExecutor, ExecutionContext, Future}
import scala.xml.Unparsed

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Interpreter extends Configuration with VesperConversions with Flattener {

  implicit def executionContext: ExecutionContextExecutor = ExecutionContext.Implicits.global

  val parser: Parser    = CommandParser()
  val storage: Storage  = VesperStorage()

  val Curator     = 0
  val Reviewer    = 1

  private def emitRoles(who:Role, question:ExactRole): Future[Option[Result]] = {
    def f(x: Int): Boolean = if(x == Reviewer) true else false
    def g(x: Int): Boolean = if(x == Curator)  true else false


    val askQuestion: Future[mutable.Buffer[String]] = Future {
      var result: mutable.Buffer[String] = mutable.Buffer.empty[String]
      question.value  match {
        case "curators"   => result ++ club.filter {case (k,v) => g(v)}.keySet.toList
        case "reviewers"  => result ++ club.filter {case (k,v) => f(v)}.keySet.toList
        case "everybody"  => result ++ passwords.keySet.toList
        case _            => result += "Unknown query!"
      }
    }

    def produceResult(answer: mutable.Buffer[String]) = Future {
      var result: Option[Result] = None
      if(who.id != Curator) {
        result = Some(Result(failure = Some(Failure("You are not authorized to see these resources."))))
      } else {
        answer.toList match {
          case first :: rest =>
            if(first == "Unknown query!") {
              result = Some(Result(failure = Some(Failure(first))))
            } else {
              result = Some(Result(info = Some(Info(answer.toList))))
            }
          case Nil => result = Some(Result(info = Some(Info(List("No roles found!")))))
        }

      }

      result

    }

    val queried = for {
      answer <- askQuestion
      result <- produceResult(answer)
    } yield {
      result
    }

    queried
  }

  private[core] def collectSource(code: Code): Future[Source] = Future {
    asSource(code)
  }

  private[core] def collectIssues(introspector: Introspector, source: Source): Future[mutable.Set[Issue]] = Future {
    try {
      asScalaSet(introspector.detectIssues(source))
    } catch {
      case e:RuntimeException =>
        mutable.Set.empty[Issue]
    }
  }


  private[core] def createChange(refactorer: Refactorer, request: ChangeRequest): Future[Change] = Future {
    refactorer.createChange(request)
  }

  private[core] def applyChange(refactorer: Refactorer, change: Change): Future[Commit] = Future {
    refactorer.apply(change)
  }

  def createIntrospector(): Future[Introspector] = Future(
    Vesper.createIntrospector()
  )

  private def evalInspect(refactorer: Refactorer, inspect: Inspect): Future[Option[Result]] = {
    try {

      def verifySourceWithIntrospector(source: Source, introspector: Introspector): Future[Seq[String]] = Future(
        asScalaBuffer(introspector.detectSyntaxErrors(source))
      )

      def collectWarnings(problems: Seq[String]): Future[Seq[Warning]] = Future(problems.map(x => Warning(x)))

      def collectImports(introspector: Introspector, source: Source): Future[Seq[String]] = Future {
        asScalaSet(introspector.detectMissingImports(source)).toSeq
      }

      def verifySource(warnings: Seq[Warning]): Future [Option[Result]] = {
        try {

          Future {
            val result: Option[Result] = warnings.isEmpty match {
              case true  =>  Some(Result(warnings = Some(List())))
              case false =>  Some(Result(warnings = Some(warnings.toList)))
            }

            result
          }

        } catch {
          case e: RuntimeException =>
            Future(Some(Result(warnings = Some(List(Warning(e.getMessage))))))
        }
      }

      def fetchImports(imports: Seq[String]): Future [Option[Result]] = {
        Future {

          val result: Option[Result] = imports.isEmpty match {
            case false  =>  Some(Result(info = Some(Info(imports.toList))))
            case true   =>  Some(Result(info = Some(Info(List()))))
          }

          result
        }
      }

      val inspected = for {
        vesperSource          <- collectSource(inspect.source)
        vesperIntrospector    <- createIntrospector()
        caughtProblems        <- verifySourceWithIntrospector(vesperSource, vesperIntrospector)
        warnings              <- collectWarnings(caughtProblems)
        imports               <- collectImports(vesperIntrospector, vesperSource)
        result                <- if(!inspect.imports) verifySource(warnings) else fetchImports(imports)
      } yield {
        result
      }

      inspected

    } catch {
      case e: Exception =>  throwFailure(e.getMessage)
    }

  }

  private def createRemoveChangeRequest(what: String, selection: SourceSelection): ChangeRequest = {
    what match {
      case "class"      => ChangeRequest.deleteClass(selection)
      case "method"     => ChangeRequest.deleteMethod(selection)
      case "field"      => ChangeRequest.deleteField(selection)
      case "parameter"  => ChangeRequest.deleteParameter(selection)
      case "region"     => ChangeRequest.deleteRegion(selection)
      case _=> throw new NoSuchElementException(what + " was not found")
    }
  }

  private def removeMember(refactorer:Refactorer, what: String, where: List[Int], source: Source): Future[Option[Result]] = {
    try {
      val createRequest: Future[ChangeRequest] = Future {
        val select: SourceSelection = new SourceSelection(source, where(0), where(1))
        createRemoveChangeRequest(what, select)
      }

      def produceResult(change: Change, commit: Commit): Future[Option[Result]] = Future {
        var result: Option[Result]  = None
        commit != null && commit.isValidCommit match {
          case true =>  result = Some(Result(draft = Some(asDraft(commit))))
          case false => result = Some(Result(failure = Some(Failure(change.getErrors.mkString(" ")))))
        }

        result
      }


      val removed = for {
        request  <- createRequest
        change   <- createChange(refactorer, request)
        commit   <- applyChange(refactorer, change)
        result   <- produceResult(change, commit)
      } yield {
        result
      }

      removed

    } catch {
      case e: Exception =>  throwFailure(e.getMessage)
    }
  }

  private def evalRemove(refactorer: Refactorer, delete: Remove): Future[Option[Result]] = {
    val what:String           = delete.what
    val where:List[Int]       = delete.where
    val vesperSource: Source  = asSource(delete.source)

    removeMember(refactorer, what, where, vesperSource)
  }


  private def evalTrim(refactorer: Refactorer, trim: Trim): Future[Option[Result]] = {
    val where:List[Int]       = trim.where
    val vesperSource: Source  = asSource(trim.source)

    clipSelection(refactorer, where, vesperSource)
  }


  private def clipSelection(refactorer:Refactorer, where: List[Int], source: Source): Future[Option[Result]] = {
    try {
      val createRequest: Future[ChangeRequest] = Future {
        val select: SourceSelection = new SourceSelection(source, where(0), where(1))
        ChangeRequest.clipSelection(select)
      }

      def produceResult(change: Change, commit: Commit): Future[Option[Result]] = Future {
        var result: Option[Result]  = None
        commit != null && commit.isValidCommit match {
          case true =>  result = Some(Result(draft   = Some(asFormattedDraft(commit))))
          case false => result = Some(Result(failure = Some(Failure(change.getErrors.mkString(" ")))))
        }

        result
      }


      val removed = for {
        request  <- createRequest
        change   <- createChange(refactorer, request)
        commit   <- applyChange(refactorer, change)
        result   <- produceResult(change, commit)
      } yield {
        result
      }

      removed

    } catch {
      case e: Exception =>  throwFailure(e.getMessage)
    }
  }

  private def createRenameChangeRequest(what: String, name: String, selection: SourceSelection): ChangeRequest = {
    what match {
      case "class"      => ChangeRequest.renameClassOrInterface(selection, name)
      case "method"     => ChangeRequest.renameMethod(selection, name)
      case "field"      => ChangeRequest.renameField(selection, name)
      case "parameter"  => ChangeRequest.renameParameter(selection, name)
      case "member"     => ChangeRequest.renameSelectedMember(selection, name)
      case _=> throw new NoSuchElementException(what + " was not found")
    }
  }

  private def renameMember(refactorer: Refactorer, what: String, name: String, where: List[Int], source: Source): Future[Option[Result]] = {
    try {

      val createRequest: Future[ChangeRequest] = Future {
        val select: SourceSelection = new SourceSelection(source, where(0), where(1))
        createRenameChangeRequest(what, name, select)
      }

      def produceResult(change: Change, commit: Commit): Future[Option[Result]] = Future {
        var result: Option[Result]  = None
        commit != null && commit.isValidCommit match {
          case true =>  result = Some(Result(draft   = Some(asFormattedDraft(commit))))
          case false => result = Some(Result(failure = Some(Failure(change.getErrors.mkString(" ")))))
        }

        result
      }


      val renamed = for {
        request  <- createRequest
        change   <- createChange(refactorer, request)
        commit   <- applyChange(refactorer, change)
        result   <- produceResult(change, commit)
      } yield {
        result
      }

      renamed

    } catch {
      case e: Exception => throwFailure(e.getMessage)
    }
  }

  private def throwFailure(message: String): Future[Option[Result]] = {
    Future(Some(Result(failure = Some(Failure(message)))))
  }

  private def evalRename(refactorer: Refactorer, rename: Rename): Future[Option[Result]] = {
    val what:String           = rename.what
    val where:List[Int]       = rename.where
    val name: String          = rename.to
    val vesperSource: Source  = asSource(rename.source)

    renameMember(refactorer, what, name, where, vesperSource)
  }

  private def evalOptimize(refactorer: Refactorer, optimize: Optimize): Future[Option[Result]] = {

    def createRequest(source: Source): Future[ChangeRequest] = Future {
      ChangeRequest.optimizeImports(source)
    }

    def produceResult(change: Change, commit: Commit): Future[Option[Result]] = Future {
      var result: Option[Result]  = None
      commit != null && commit.isValidCommit match {
        case true =>  result = Some(Result(draft   = Some(asFormattedDraft(commit))))
        case false => result = Some(Result(failure = Some(Failure(change.getErrors.mkString(" ")))))
      }

      result
    }


    val optimized = for {
      source   <- collectSource(optimize.source)
      request  <- createRequest(source)
      change   <- createChange(refactorer, request)
      commit   <- applyChange(refactorer, change)
      result   <- produceResult(change, commit)
    } yield {
      result
    }

    optimized
  }

  private def evalFormat(refactorer: Refactorer, format: Format): Future[Option[Result]] = {

    def createRequest(source: Source): Future[ChangeRequest] = Future {
      ChangeRequest.reformatSource(source)
    }

    def produceResult(change: Change, commit: Commit): Future[Option[Result]] = Future {
      var result: Option[Result]  = None
      commit != null && commit.isValidCommit match {
        case true =>  result = Some(Result(draft   = Some(asFormattedDraft(commit))))
        case false => result = Some(Result(failure = Some(Failure(change.getErrors.mkString(" ")))))
      }

      result
    }

    val reformatted = for {
      source   <- collectSource(format.source)
      request  <- createRequest(source)
      change   <- createChange(refactorer, request)
      commit   <- applyChange(refactorer, change)
      result   <- produceResult(change, commit)
    } yield {
      result
    }

    reformatted
  }


  private def evalCleanup(refactorer: Refactorer, cleanup: Cleanup): Future[Option[Result]] = {

    def detectIssues(source: Source): Future[mutable.Set[Issue]] = Future {
      val introspector: Introspector = Vesper.createIntrospector()
      val result:mutable.Set[Issue] = asScalaSet(introspector.detectIssues(source))

      val issues: mutable.Set[Issue] = result.filter(
        i => Names.hasAvailableResponse(i.getName.asInstanceOf[Smell])
          && Names.from(i.getName.asInstanceOf[Smell]).isSame(Refactoring.DEDUPLICATE))


      issues
    }


    def createCommit(issue: Issue): Commit = {
      val refactor: Refactorer = Vesper.createRefactorer()
      val change: Change = refactor.createChange(ChangeRequest.forIssue(issue))
      val commit: Commit = refactor.apply(change)

      commit
    }

    def optimize(refactorer: Refactorer, code: Source): Future[Source] = Future {
      val request: ChangeRequest = ChangeRequest.optimizeImports(code)
      val change: Change = refactorer.createChange(request)
      val commit: Commit = refactorer.apply(change)
      if(commit != null && commit.isValidCommit){
        commit.getSourceAfterChange
      } else {
        commit.getSourceBeforeChange
      }
    }


    def reformat(refactorer: Refactorer, code: Source): Future[Source] = Future {
      val request: ChangeRequest = ChangeRequest.reformatSource(code)
      val change: Change = refactorer.createChange(request)
      val commit: Commit = refactorer.apply(change)
      if(commit != null && commit.isValidCommit){
        commit.getSourceAfterChange
      } else {
        commit.getSourceBeforeChange
      }
    }

    def deduplicate(refactorer: Refactorer, before: Source, issues: mutable.Set[Issue]): Future[Source] = Future {
      // http://stackoverflow.com/questions/24571444/get-element-from-set

      val commit: Commit = if (issues.isEmpty) null else createCommit(issues.toSeq(0))
      if(commit != null && commit.isValidCommit) {
         commit.getSourceAfterChange
      } else {
         before
      }
    }


    def produceResult(before: Source, after: Source): Future[Option[Result]] = Future {
      Some(
        Result(
          draft = Some(
            asFormatterDraft(
              before,
              after,
              "Full cleanup",
              "Reformatted code and also removed code redundancies"
            )
          )
        ))
    }


    val cleanedUp = for {
      source       <- collectSource(cleanup.source)
      formatted    <- reformat(refactorer, source)
      optimized    <- optimize(refactorer, formatted)
      issues       <- detectIssues(optimized)
      deduplicated <- deduplicate(refactorer, optimized, issues)
      result       <- produceResult(formatted, deduplicated)
    } yield {
      result
    }

    cleanedUp
  }

  private def evalDeduplicate(refactorer: Refactorer, deduplicate: Deduplicate): Future[Option[Result]] = {

    def filterNonDeduplicateIssues(issues:mutable.Set[Issue]): Future[mutable.Set[Issue]] = Future {
       issues.filter(i => Names.hasAvailableResponse(i.getName.asInstanceOf[Smell]) && Names.from(i.getName.asInstanceOf[Smell]).isSame(Refactoring.DEDUPLICATE))
    }


    def applyChanges(filteredIssues:mutable.Set[Issue]): Future[mutable.Buffer[Commit]] = Future {
      val commits: mutable.Buffer[Commit] = mutable.Buffer.empty[Commit]
      for(i <- filteredIssues){
        val change: Change = refactorer.createChange(ChangeRequest.forIssue(i))
        val commit: Commit = refactorer.apply(change)
        commits += commit
      }

      commits
    }


    def produceResult(commits: mutable.Buffer[Commit]): Future[Option[Result]] = Future {
      var result: Option[Result]  = Some(Result(info  = Some(Info(List("everything looks clear!")))))

      for(c <- commits){
        c != null && c.isValidCommit match {
          case true   =>  result = Some(Result(draft   = Some(asDraft(c))))
          case false  =>  result = Some(Result(failure = Some(Failure("invalid commit"))))
        }
      }

      result
    }

    val deduplicated = for {
      source   <- collectSource(deduplicate.source)
      intros   <- createIntrospector()
      issues   <- collectIssues(intros, source)
      fIssues  <- filterNonDeduplicateIssues(issues)
      changes  <- applyChanges(fIssues)
      result   <- produceResult(changes)
    } yield {
      result
    }

    deduplicated

  }


  private def evalFind(who: Role, find: Find) : Future[Option[Result]] = {

    val answer = flatten(find)

    answer match {
      case role: ExactRole              => emitRoles(who, role)
      case all: AllInSet                => storage.findAll()
      case any: AnyInSet                => storage.find(any)
      case exact: ExactlyOne            => storage.find(exact)
      case exactlyAll: ExactlyAllInSet  => storage.find(exactlyAll)
      case byId: ById                   => storage.findById(byId)
      case _                            => Future(Some(Result(failure = Some(Failure("Unknown command")))))
    }
  }

  private def limitWords(builder: StringBuilder, words: List[String], visited: mutable.Set[String], limit: Int) = {

    val BLANK     = " "

    for(w <- words){
      val tweetCount = builder.size
      val remainder  = limit - tweetCount

      val ht        = "#" + w
      val lookAhead = remainder - (BLANK + ht).length
      if(lookAhead >= 0 && !visited.contains(ht)){
        builder.append(BLANK).append(ht)
        visited.add(ht)
      }
    }
  }


  private def evalPersist(who:Auth, persist: Persist): Future[Option[Result]] = {
    def makeVesperUrl(id: Option[String]): Future[String] = id match {
      case Some(cid) => Future("""http://www.vesperin.com/kiwi/render?q=id:""" + cid)
      case None      => Future("""http://www.vesperin.com/kiwi/help""")
    }

    def getDescription(code: Code): Future[String] = Future(code.description)

    def getVesperUrl(code: Code): Future[String]   = {
      val googleShortener: GoogleUrlShortener = new GoogleUrlShortener
      for {
        vesperUrl <- makeVesperUrl(code.id)
        tinyUrl   <- googleShortener.shortenUrl(vesperUrl)
      } yield {
        tinyUrl
      }
    }

    def buildStatus(code: Code, desc: String, tinyUrl: String): Future[String] = {
      Future {
        val ELLIPSES_TEXT: String         = "..."
        val description: String           = desc.stripPrefix("Java:").stripSuffix(".").trim
        val shortDescription: String      = if (description.length + tinyUrl.length + 2/*spaces*/ >= 100)
          description.substring(0, 50) + ELLIPSES_TEXT
        else description

        val massagedDescription: String   = shortDescription + " " + tinyUrl + " "

        val algorithms      = code.algorithms
        val datastructures  = code.datastructures
        val tags            = code.tags
        val confidence      = code.confidence match {
          case 1 => "#1star"
          case 2 => "#2star"
          case 3 => "#3star"
          case 4 => "#4star"
          case 5 => "#5star"
        }

        val builder = new mutable.StringBuilder(massagedDescription)
        val visited = mutable.Set.empty[String]

        builder.append("#Java ")
        builder.append(confidence)


        limitWords(builder, algorithms, visited, 140)
        limitWords(builder, datastructures, visited, 140)
        limitWords(builder, tags, visited, 140)

        builder.toString()

      }
    }

    def tweetMessage(code: Code): Future[String] = {
      for {
        desc          <- getDescription(code)
        tinyUrl       <- getVesperUrl(code)
        builtStatus   <- buildStatus(code, desc, tinyUrl)
      } yield {
        builtStatus
      }
    }

    def logStatus(code: Code, status: Status): Future[Code] = {
       Future {
         println("%s was saved, then tweeted by @codetour. Tweet: %s".format(code.name, status.getText))
         code
       }
    }

    def updateStatus(twitter: Twitter, statusMessage: String): Future[Status] = Future(twitter.updateStatus(statusMessage))

    def postStatus(code: Code, statusMessage: String): Future[Code] = {

      val twitter:Twitter = new TwitterFactory(
        new ConfigurationBuilder()
          .setOAuthConsumerKey(OAuthConsumerKey)
          .setOAuthConsumerSecret(OAuthConsumerSecret)
          .setOAuthAccessToken(OAuthAccessToken)
          .setOAuthAccessTokenSecret(OAuthAccessTokenSecret)
          .build()
      ).getInstance()

      for {
        status  <- updateStatus(twitter, statusMessage)
        theCode <- logStatus(code, status)
      } yield {
        theCode
      }
    }

    def tryTweeting(code: Code): Future[Code] = {
      for {
        status  <- tweetMessage(code)
        theCode <- postStatus(code, status)
      } yield {
        theCode
      }
    }

    def produceResult(theCode: Code): Future[Option[Result]] = {
      Future(Some(Result(info = Some(Info(List("%s was saved, then tweeted by @codetour".format(theCode.name)))))))
    }


    try {
      for {
        code        <- storage.persist(persist.source)
        tweetedCode <- tryTweeting(code)
        result      <- produceResult(tweetedCode)
      } yield {
        result
      }
    } catch {
      case e: Exception =>  throwFailure(e.getMessage)
    }

  }

  private[core] def unknownCommand(): Future[Option[Result]] = Future{Some(Result(failure = Some(Failure("Unknown command!"))))}

  def eval(membership: Membership, command: String) : Future[Option[Result]] = eval(membership, parser.parse(command))


  def render(command: String, survey: String): Future[Unparsed] = {
    if(!command.contains("id:")) {
      return Future(ohSnap())
    }

    def getSurveyValue(survey: String): Future[Boolean] = {
      Future {
        val value: Boolean = survey match {
          case "on" => true
          case _    => false
        }

        value
      }
    }

    def createRenderer(): Future[HtmlRenderer] = Future(CodeHtmlRenderer())

    for {
      surveyVal   <- getSurveyValue(survey)
      theResult   <- eval(command)
      renderer    <- createRenderer()
      theCodeHtml <- renderer.renderHtml(theResult, surveyVal)
    } yield {
      theCodeHtml
    }
  }

  def eval(command: String): Future[Option[Result]] = {
    eval(Membership(Auth("legolas", passwords("legolas")), Role(Curator, "legolas")), command)
  }

  def eval(membership: Membership, command: Command): Future[Option[Result]] = {
    val who: Auth               = membership.auth
    val what: Role              = membership.role

    val environment: Refactorer = Vesper.createRefactorer()

    val answer = flatten(command)

    answer match {
      case inspect:Inspect          => evalInspect(environment, inspect)
      case remove:Remove            => evalRemove(environment, remove)
      case rename:Rename            => evalRename(environment, rename)
      case optimize:Optimize        => evalOptimize(environment, optimize)
      case format:Format            => evalFormat(environment, format)
      case deduplicate:Deduplicate  => evalDeduplicate(environment, deduplicate)
      case cleanup:Cleanup          => evalCleanup(environment, cleanup)
      case find: Find               => evalFind(what, find)
      case persist: Persist         => evalPersist(who, persist)
      case trim: Trim               => evalTrim(environment, trim)
      case _                        => unknownCommand()
    }
  }

  def ohSnap() = {
    Html.ohSnap()
  }

  def renderStatusPage() =  {
     Html.renderStatusPage()
  }

}


case class VesperInterpreter() extends Interpreter

