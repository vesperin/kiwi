package com.vesperin.kiwi.interpret

import com.vesperin.kiwi.auth.{Auth, Membership, Role}
import com.vesperin.kiwi.config.Configuration
import com.vesperin.kiwi.database.{MongoStorage, Storage}
import com.vesperin.kiwi.domain.{Code, _}
import com.vesperin.kiwi.spi._
import edu.ucsc.refactor._
import edu.ucsc.refactor.spi._
import edu.ucsc.refactor.util.{Locations, SourceFormatter, StringUtil}
import twitter4j.conf.ConfigurationBuilder
import twitter4j.{Status, Twitter, TwitterFactory}

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.xml.Unparsed

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Interpreter extends Configuration with VesperLibraryConversions {

  implicit def executionContext: ExecutionContextExecutor = ExecutionContext.Implicits.global

  val parser: Parser    = CommandParser()
  val storage: Storage  = MongoStorage()

  val Curator     = 0
  val Reviewer    = 1

  val name: String = "Scratched"

  val flattener: Flattener = ResultFlattener()

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

  private[interpret] def collectSource(code: Code, preprocess: Boolean = false): Future[Source] = Future {
    val result: Source = preprocess match {
      case true  =>
        val intro: Introspector = Vesper.createIntrospector()
        val a: Source = asSource(code)
        val header: String = Source.missingHeader(intro, a, name)

        val src: Source = Source.wrap(a, name, header)
        asFormattedSource(src)
      case false => asSource(code)
    }

    result
  }


  private[interpret] def createSelection(source: Source, where: List[Int], preprocess:Boolean): Future[SourceSelection] = Future {
    val select: SourceSelection = preprocess match {
      case true  => Vesper.createAdjustedSelection(where(0), where(1), source)
      case false => new SourceSelection(source, where(0), where(1))
    }

    select
  }

  private[interpret] def createRenderer(): Future[Renderer] = Future(HtmlRenderer())

  private[interpret] def collectIssues(introspector: Introspector, source: Source): Future[mutable.Set[Issue]] = Future {
    try {
      asScalaSet(introspector.detectIssues(source))
    } catch {
      case e:RuntimeException =>
        mutable.Set.empty[Issue]
    }
  }


  private[interpret] def createChange(refactorer: Refactorer, request: ChangeRequest): Future[Change] = Future {
    refactorer.createChange(request)
  }

  private[interpret] def applyChange(refactorer: Refactorer, change: Change): Future[Commit] = Future {
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

  private def createRequest(selection: SourceSelection, what: String): Future[ChangeRequest] = Future {
    createRemoveChangeRequest(what, selection)
  }


  private def createRequest(selection: SourceSelection, what: String, name: String): Future[ChangeRequest] = Future {
    createRenameChangeRequest(what, name, selection)
  }

  private def createRequest(selection: SourceSelection): Future[ChangeRequest] = Future {
    ChangeRequest.clipSelection(selection)
  }

  private def produceResult(change: Change, before: Source, commit: Commit, preprocess:Boolean): Future[Option[Result]] = Future {
    var result: Option[Result]  = None
    commit != null && commit.isValidCommit match {
      case true => preprocess match {
        case true  =>
          result = Some(
            Result(
              draft = Some(
                asFormattedDraft(
                  before,
                  Source.unwrap(
                    commit.getSourceAfterChange,
                    Source.currentHeader(
                      commit.getSourceAfterChange,
                      StringUtil.extractFileName(commit.getSourceAfterChange.getName)
                    )
                  ),
                  commit.getNameOfChange.getKey,
                  description(commit.getNameOfChange.getKey)
                )
              )
            )
          )
        case false =>
          result = Some(Result(draft = Some(asFormattedDraft(commit))))
      }
      case false => result = Some(Result(failure = Some(Failure(change.getErrors.mkString(" ")))))
    }

    result
  }

  private def removeMember(refactorer:Refactorer, what: String, where: List[Int], code: Code, preprocess: Boolean): Future[Option[Result]] = {
    try {

      val removed = for {
        before    <- collectSource(code)
        selection <- createSelection(before, where, preprocess)
        request   <- createRequest(selection, what)
        change    <- createChange(refactorer, request)
        commit    <- applyChange(refactorer, change)
        result    <- produceResult(change, before, commit, preprocess)
      } yield {
        result
      }

      removed

    } catch {
      case e: Exception =>  throwFailure(e.getMessage)
    }
  }

  private def evalRemove(refactorer: Refactorer, delete: Remove): Future[Option[Result]] = {
    val what:String             = delete.what
    val where:List[Int]         = delete.where
    val preprocessing: Boolean  = delete.preprocess
    val code: Code              = delete.source

    removeMember(refactorer, what, where, code, preprocessing)
  }


  private def evalMultistage(stage: Multistage): Future[Option[Result]] = {

    def generateStages(introspector: Introspector, source: Source): Future[java.util.List[Clip]] = Future {
      introspector.multiStage(source)
    }

    def summarizeStages(introspector: Introspector, budget:Int, stages: java.util.List[Clip]): Future[java.util.Map[Clip, java.util.List[Location]]] = Future {
      introspector.summarize(stages, budget)
    }

    def convertJavaMapToScalaMap(stages: java.util.Map[Clip, java.util.List[Location]],
        preprocess:Boolean = false): Future[Map[Clip, List[Location]]] = Future {

      val javaMap = preprocess match {
        case true  => CodeIntrospector.adjustClipspace(stages)
        case false => stages
      }

      javaMap.mapValues(_.toList).toMap
    }


    def produceResult(stages: Map[Clip, List[Location]], budget: Int): Future[Option[Result]] =  Future {

      def toList(loc: List[Location]): List[Block] = {
        val stages: mutable.Buffer[Block] = mutable.Buffer.empty[Block]
        for( l <- loc){
          //l.getStart.getOffset, l.getEnd.getOffset
          val fromLine:Int = l.getStart.getLine
          val fromChr:Int  = l.getStart.getOffset
          val toLine:Int   = l.getEnd.getLine
          val toChr:Int    = l.getEnd.getOffset
          val crop:String  = l.getSource.getContents.substring(fromChr,toChr)

          stages += Block(crop, List(fromLine,fromChr), List(toLine,toChr))
        }

        stages.toList
      }

      val result: mutable.Buffer[Stage] = mutable.Buffer.empty[Stage]
      for((k, v) <- stages){
        result += Stage(
          k.getLabel,
          k.getMethodName,
          k.isBaseClip,
          toList(v).sortBy(_.from(0)),
          asCode(k.getSource),
          budget = budget
        )
      }

      Some(
        Result(
          stages = Some(
            MultiStaged(
              result.sortBy(_.source.content.length).toList
            )
          )
        )
      )
    }

    val multiStaged = for {
      source         <- collectSource(stage.source, stage.preprocess)
      introspector   <- createIntrospector()
      stages         <- generateStages(introspector, source)
      summaries      <- summarizeStages(introspector, stage.budget, stages)
      scalaSummaries <- convertJavaMapToScalaMap(summaries, stage.preprocess)
      result         <- produceResult(scalaSummaries, stage.budget)
    } yield {
      result
    }

    multiStaged
  }

  private def evalSummarize(summarize: Summarize): Future[Option[Result]] = {
    def foldableLocations(introspector: Introspector, stage: Stage, src: Source, preprocess: Boolean): Future[List[Location]] = Future {

      val resultList = preprocess match {
        case true  =>
          val folds = introspector.summarize(stage.method, src, stage.budget)

          val adjustedSrc: Source = Source.unwrap(src)

          Locations.adjustLocations(folds, adjustedSrc)
        case false =>
          introspector.summarize(stage.method, src, stage.budget)
      }

      asScalaBuffer(resultList).toList
    }

    def toList(loc: List[Location]): Future[List[Block]] = Future {
      val stages: mutable.Buffer[Block] = mutable.Buffer.empty[Block]
      for( l <- loc){
        //l.getStart.getOffset, l.getEnd.getOffset
        val fromLine:Int = l.getStart.getLine
        val fromChr:Int  = l.getStart.getOffset
        val toLine:Int   = l.getEnd.getLine
        val toChr:Int    = l.getEnd.getOffset
        val crop:String  = l.getSource.getContents.substring(fromChr,toChr)

        stages += Block(crop, List(fromLine,fromChr), List(toLine,toChr))
      }

      stages.toList
    }

    def summarizeCode(stage: Stage, locations: List[Block]): Future[Option[Result]] = Future {
       Some(
         Result(
           stage = Some(
             Stage(
               stage.label,
               stage.method,
               stage.isBase,
               locations,
               stage.source,
               stage.budget
             )
           )
         )
       )
    }

    val summary =  for {
      introspector <- createIntrospector()
      source       <- collectSource(summarize.stage.source, summarize.preprocess)
      foldable     <- foldableLocations(introspector, summarize.stage, source, summarize.preprocess)
      locations    <- toList(foldable)
      sum          <- summarizeCode(summarize.stage, locations)
    } yield {
      sum
    }

    summary
  }


  private def evalSlice(refactorer: Refactorer, slice: Slice): Future[Option[Result]] = {
    val where:List[Int]      = slice.where
    val preprocess: Boolean  = slice.preprocess
    val code: Code           = slice.source

    clipSelection(refactorer, where, code, preprocess)
  }


  private def clipSelection(refactorer:Refactorer, where: List[Int], code: Code, preprocess: Boolean): Future[Option[Result]] = {
    try {

      val removed = for {
        before    <- collectSource(code)
        selection <- createSelection(before, where, preprocess)
        request   <- createRequest(selection)
        change    <- createChange(refactorer, request)
        commit    <- applyChange(refactorer, change)
        result    <- produceResult(change, before, commit, preprocess)
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

  private def renameMember(refactorer: Refactorer, what: String, name: String, where: List[Int], code: Code, preprocess: Boolean): Future[Option[Result]] = {
    try {

      val renamed = for {
        before    <- collectSource(code)
        selection <- createSelection(before, where, preprocess)
        request   <- createRequest(selection, what, name)
        change    <- createChange(refactorer, request)
        commit    <- applyChange(refactorer, change)
        result    <- produceResult(change, before, commit, preprocess)
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

    val what:String             = rename.what
    val name: String            = rename.to
    val where:List[Int]         = rename.where
    val preprocessing: Boolean  = rename.preprocess
    val code: Code              = rename.source

    renameMember(refactorer, what, name, where, code, preprocessing)
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

    def produceResult(before: Source, after: Source): Future[Option[Result]] = Future {
      Some(
        Result(
          draft = Some(
            asFormattedDraft(
              before,
              after,
              "Reformat Code",
              "Reformatted code"
            )
          )
        ))
    }

    val reformatted = for {
      source    <- collectSource(format.source)
      formatted <- reformat(source)
      result    <- produceResult(source, formatted)
    } yield {
      result
    }

    reformatted
  }


  private def reformat(code: Source): Future[Source] = Future {
    val formattedContent: String      = new SourceFormatter().format(code.getContents)
    val formattedToGoogleStyle:Source = Source.from(code, formattedContent)

    formattedToGoogleStyle
  }


  private def optimize(refactorer: Refactorer, code: Source, preprocess: Boolean = false): Future[Source] = Future {

    val result: Source = preprocess match {
      case true  => code // if true, then we don't need to optimize imports
      case false =>
        val request: ChangeRequest = ChangeRequest.optimizeImports(code)
        val change: Change = refactorer.createChange(request)
        val commit: Commit = refactorer.apply(change)
        if(commit != null && commit.isValidCommit){
          commit.getSourceAfterChange
        } else {
          commit.getSourceBeforeChange
        }
    }

    result
  }


  private def filterNonDeduplicateIssues(issues:mutable.Set[Issue]): Future[mutable.Set[Issue]] = Future {
    issues.filter(i => Names.hasAvailableResponse(i.getName.asInstanceOf[Smell])
      && Names.from(i.getName.asInstanceOf[Smell]).isSame(Refactoring.DEDUPLICATE))
  }


  private def applyChanges(refactorer: Refactorer, filteredIssues:mutable.Set[Issue]): Future[mutable.Buffer[Commit]] = Future {
    val commits: mutable.Buffer[Commit] = mutable.Buffer.empty[Commit]
    for(i <- filteredIssues){
      val change: Change = refactorer.createChange(ChangeRequest.forIssue(i))
      val commit: Commit = refactorer.apply(change)
      commits += commit
    }

    commits
  }


  private def evalCleanup(refactorer: Refactorer, cleanup: Cleanup): Future[Option[Result]] = {

    def collectSourceAfterChanges(before: Source, changes: mutable.Buffer[Commit]): Future[Source] = Future {
      // http://stackoverflow.com/questions/24571444/get-element-from-set

      val commit: Commit = if (changes.isEmpty) null else changes.toSeq(0)
      val source: Source = commit != null && commit.isValidCommit match {
        case true  => commit.getSourceAfterChange
        case false => before
      }

      source
    }

    def unwrappedCode(source: Source, preprocess:Boolean): Future[Source] =  Future {
      preprocess match {
        case true =>
          Source.unwrap(
            source,
            Source.currentHeader(
              source,
              StringUtil.extractFileName(source.getName)
            )
          )
        case false =>
          source
      }
    }


    def produceResult(before: Source, after: Source, preprocess: Boolean): Future[Option[Result]] = Future {
      Some(
        Result(
          draft = Some(
            asFormattedDraft(
              before,
              after,
              "Full cleanup",
              "Reformatted code and also removed code redundancies"
            )
          )
        ))
    }


    val cleanedUp = for {
      before       <- collectSource(cleanup.source)
      preprocessed <- collectSource(cleanup.source, cleanup.preprocess)
      optimized    <- optimize(refactorer, preprocessed, cleanup.preprocess)
      introspector <- createIntrospector()
      issues       <- collectIssues(introspector, optimized)
      duplicates   <- filterNonDeduplicateIssues(issues)
      changes      <- applyChanges(refactorer, duplicates)
      collected    <- collectSourceAfterChanges(optimized, changes)
      after        <- unwrappedCode(collected, cleanup.preprocess)
      result       <- produceResult(before, after, cleanup.preprocess)
    } yield {
      result
    }

    cleanedUp
  }

  private def evalDeduplicate(refactorer: Refactorer, deduplicate: Deduplicate): Future[Option[Result]] = {

    def produceResult(commits: mutable.Buffer[Commit], preprocess: Boolean): Future[Option[Result]] = Future {
      var result: Option[Result]  = Some(Result(info  = Some(Info(List("everything looks clear!")))))

      for(c <- commits){
        c != null && c.isValidCommit match {
          case true   => preprocess match {
            case true  =>
              result = Some(
                Result(
                  draft = Some(
                    asFormattedDraft(
                      Source.unwrap(
                        c.getSourceBeforeChange,
                        Source.currentHeader(
                          c.getSourceBeforeChange,
                          StringUtil.extractFileName(c.getSourceBeforeChange.getName)
                        )
                      ),
                      Source.unwrap(
                        c.getSourceAfterChange,
                        Source.currentHeader(
                          c.getSourceAfterChange,
                          StringUtil.extractFileName(c.getSourceAfterChange.getName)
                        )
                      ),
                      c.getNameOfChange.getKey,
                      description(c.getNameOfChange.getKey)
                    )
                  )
                )
              )
            case false =>
              result = Some(Result(draft = Some(asFormattedDraft(c))))
          }
          case false  =>  result = Some(Result(failure = Some(Failure("invalid commit"))))
        }
      }

      result
    }

    val deduplicated = for {
      source   <- collectSource(deduplicate.source, deduplicate.preprocess)
      intros   <- createIntrospector()
      issues   <- collectIssues(intros, source)
      fIssues  <- filterNonDeduplicateIssues(issues)
      changes  <- applyChanges(refactorer, fIssues)
      result   <- produceResult(changes, deduplicate.preprocess)
    } yield {
      result
    }

    deduplicated

  }


  private def evalFind(who: Role, find: Find) : Future[Option[Result]] = {

    val answer = flattener.flatten(find)

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


  private def evalPreprocessing(preprocess: Preprocess): Future[Option[Result]] = {

    def detectsPartialSnippet(introspector:Introspector, source: Source): Future[Boolean] =
      Future(introspector.detectPartialSnippet(source))

    def produceResult(answer: Boolean): Future[Option[Result]] = Future {
      Some(Result(info = Some(Info(List(answer.toString)))))
    }

    val preprocessed = for {
      source    <- collectSource(preprocess.source)
      intro     <- createIntrospector()
      answer    <- detectsPartialSnippet(intro, source)
      result    <- produceResult(answer)
    } yield {
      result
    }

    preprocessed
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



  private def getId(id: Option[String]): String = id match {
    case Some(cid) => cid
    case None      => "none"
  }


  private def evalPersist(who:Auth, persist: Persist): Future[Option[Result]] = {
    def makeVesperUrl(id: Option[String]): Future[String] = id match {
      case Some(cid) => Future("""http://www.vesperin.com/kiwi/render?q=id:""" + cid)
      case None      => Future("""http://www.vesperin.com/kiwi/help""")
    }

    def getDescription(code: Code): Future[String] = Future(code.description)

    def getVesperUrl(code: Code): Future[String]   = {
      val googleShortener: UrlShortener = new UrlShortener
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

    def tweetMessage(code: Code, tinyUrl: String): Future[String] = {
      for {
        desc          <- getDescription(code)
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

    def tryTweeting(code: Code, tinyUrl: String): Future[Code] = {
      for {
        status  <- tweetMessage(code, tinyUrl)
        theCode <- postStatus(code, status)
      } yield {
        theCode
      }
    }

    def produceResult(theCode: Code, tinyUrl: String, id: Option[String]): Future[Option[Result]] = {
      Future(Some(Result(info = Some(Info(List(
        "%s was saved, then tweeted by @codetour".format(theCode.name),
        tinyUrl,
        getId(id)
      ))))))
    }


    try {
      for {
        code        <- storage.persist(persist.source)
        tinyUrl     <- getVesperUrl(code)
        tweetedCode <- tryTweeting(code, tinyUrl)
        result      <- produceResult(tweetedCode, tinyUrl, code.id)
      } yield {
        result
      }
    } catch {
      case e: Exception =>  throwFailure(e.getMessage)
    }

  }

  private[interpret] def unknownCommand(): Future[Option[Result]] = Future{Some(Result(failure = Some(Failure("Unknown command!"))))}

  def eval(membership: Membership, command: String) : Future[Option[Result]] = eval(membership, parser.parse(command))


  def render(command: String, survey: String): Future[Unparsed] = {
    if(!command.contains("id:")) {
      return errorPage()
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

    val answer = flattener.flatten(command)

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
      case preprocess: Preprocess   => evalPreprocessing(preprocess)
      case slice: Slice             => evalSlice(environment, slice)
      case stage: Multistage        => evalMultistage(stage)
      case summarize: Summarize     => evalSummarize(summarize)
      case _                        => unknownCommand()
    }
  }


  def errorPage(): Future[Unparsed] = {
    for {
      renderer    <- createRenderer()
      theCodeHtml <- renderer.renderError()
    } yield {
      theCodeHtml
    }
  }



  def statusPage(): Future[Unparsed]  =  {
    for {
      renderer    <- createRenderer()
      theCodeHtml <- renderer.renderStatusHtml()
    } yield {
      theCodeHtml
    }
  }

}


case class CommandInterpreter() extends Interpreter

