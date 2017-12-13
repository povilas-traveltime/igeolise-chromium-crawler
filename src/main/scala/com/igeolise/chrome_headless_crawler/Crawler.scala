package com.igeolise.chrome_headless_crawler

import java.io.File

import crawler.command_parser._
import io.webfolder.cdp.Launcher

import io.webfolder.cdp.session.{Session, SessionFactory}
import io.webfolder.cdp.session.Extensions._

import scala.collection.JavaConversions._
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

case class CrawlerResult(successes: Seq[File], failures: Seq[Throwable])
case class CrawlerException(message: String) extends Exception(message)
case class CrawlerCaughtException(message: String, exception: Throwable) extends Exception(s"$message:\n${exception.getMessage}", exception)

class Crawler(sessionFactory: SessionFactory, pageWaitTimeout: Int) extends CrawlerHelpers {

  private val log = LoggerFactory.getLogger(classOf[Crawler])



  private case class State(elementStack: Seq[Int], doneActions: Seq[Action], pendingActions: Seq[Action], target: File, session: Session, result: Option[File], startUrl: String) {
    def getStackTop: Try[Int] = Try { elementStack.headOption.getOrElse(throw CrawlerException("Element stack empty")) }
    def appendStack(id: Int): State = this.copy(id +: elementStack)
    def stackTail: State = this.copy(elementStack.tail)
    def moveActionToDone: State = {
      val action = pendingActions.head
      this.copy(doneActions = this.doneActions :+ action, pendingActions = this.pendingActions.tail)
    }
  }

  private def executeActions(states: Seq[State]): Seq[Try[State]] = {
    states.flatMap { s =>
      val executed = s.pendingActions.headOption.map { a =>
        executeAction(a, s) match {
          case Success(seq) =>
            seq.map(s => Try { s.moveActionToDone } )
          case e: Failure[Seq[State]] => Seq(e.map(_.head))
        }
      }
      executed.getOrElse(Seq(Try { s }))
    }
  }

  private def createState(session: Session, actionSequence: Seq[Action], downloadTarget: File, url: String): Try[State] = {
    val initState = (documentId: Int) => Try {
      State(Seq(documentId), Seq.empty, actionSequence, downloadTarget, session, None, url)
    }
      .recover { case e: Throwable =>
        session.close()
        throw e
      }
    for {
      _           <- waitForPage(_.navigate(url), pageWaitTimeout)(session)
      documentId  <- getDocumentNodeId(session)
      state       <- initState(documentId)
    } yield state
  }

  private def remapStateWithoutForAll(state: State, factory: SessionFactory, actionToInsert: Action): Try[State] = {
    val actionList = state.doneActions ++  (actionToInsert +: state.pendingActions.tail)
    val newSession = factory.create()
    createState(newSession, actionList, state.target, state.startUrl)
  }

  def crawl(commands: Seq[Action], downloadLocation: File): CrawlerResult = {
    log.info("Navigation started.")
    commands.headOption match {
      case Some(NavigateTo(url)) =>
        val session = sessionFactory.create()

        val result = for {
          state       <- createState(session, commands.tail, downloadLocation, url)
          result_     <- Try { executeActions(Seq(state)) }
        } yield result_

        result match {
          case Success(r) => manageResult(r)
          case Failure(e) => manageResult(Seq(Try(throw CrawlerCaughtException("Problem while initializing",e))))
        }

      case Some(NavigateToDownload(url, credentials)) =>
        val session = sessionFactory.create()
        val result = for {
          _ <- setCredentials(session, credentials)
          result_ <- download(session, _.navigate(url), downloadLocation, pageWaitTimeout)
        } yield result_

        result match {
          case Success(r) => CrawlerResult(Seq(r), Seq.empty)
          case Failure(e) => CrawlerResult(Seq.empty, Seq(e))
        }

      case Some(_) => CrawlerResult(Seq.empty, Seq(CrawlerException(s"First command does not navigate to a page:\n" +
        commands.map(_.toString).mkString("\\"))))
      case None => CrawlerResult(Seq.empty, Seq(CrawlerException("Action list empty")))
    }
  }

  private def manageResult(states: Seq[Try[State]]): CrawlerResult = {
    states.foldLeft(CrawlerResult(Seq.empty, Seq.empty)) {
      case (result, Success(State(_, _, _, _, _, Some(file), _))) =>
        result.copy(file +: result.successes)
      case (result, Success(State(_, doneActions, _, _, _, None, _))) =>
        result.copy(failures = CrawlerException(s"Failed to download any file by running script: ${doneActions.map(_.toString).mkString("\\")}") +: result.failures)
      case (result, Failure(e)) => result.copy(failures = e +: result.failures)
    }
  }

  private def executeAction(action: Action, state: State): Try[Seq[State]] = {

    val session = state.session

    action match {
      case In(element) =>
        for {
          id      <- getNodeId(session, elementToSelector(element))
          result  <- Try { Seq(state.appendStack(id))}
        } yield result
      case Click =>
        for {
          id      <- state.getStackTop
          _       <- waitForPage(_.clickDom(id), pageWaitTimeout) (session)
          result  <- Try { Seq(state) }
        } yield result
      case ClickDownload =>
        for {
          nodeId    <- state.getStackTop
          download  <- download(session, _.clickDom(nodeId), state.target, pageWaitTimeout)
          result    <- Try { Seq(state.copy(result = Some(download))) }
        } yield result
      case OnCurrentPage =>
        for {
          pageId  <- getDocumentNodeId(session)
          result  <- Try { Seq(state.copy(elementStack = Seq(pageId))) }
        } yield result
      case Up =>
        Try { Seq(state.stackTail) }
      case TypeIn(text) =>
        val rest = (nodeId: Int) => trySessionWithFailureMessage("While typing", session) { ses =>
          ses.focus(nodeId)
          ses.sendKeys(text)
          Seq(state)
        }
        for {
          nodeId <- state.getStackTop
          result <- rest(nodeId)
        } yield result
      case ForAllElems(element) =>
        val getNodeIds = (parrentId: Int) => trySessionWithFailureMessage("While getting node Ids", session) { ses =>
          ses.getNodeIds(elementToSelector(element))
        }
        for {
          parentNodeId  <- state.getStackTop
          nodes         <- getNodeIds(parentNodeId)
          attributes    <- Try { nodes.map(n => getNodeAttributes(session, n)).filter(_.isSuccess).map(_.get) }
          elements      <- Try { attributes.map(a => mapAttributesToElement(a)) }
          newStates     <- Try { elements.map(e=> remapStateWithoutForAll(state, sessionFactory, In(e)).get) }
        } yield newStates

      case FindContainingInLastResult(text) =>
        for {
          nodes   <- trySessionWithFailureMessage("While getting node Ids", session) { _.getNodeIds(s"a:contains('$text')") }
          _       <- Try { nodes.map(n => trySessionWithFailureMessage("While clicking link", session) { _.clickDom(n) } ) }
          result  <- Try { Seq(state) }
        } yield result

      case NavigateTo(url) =>
        for {
          _           <- trySessionWithFailureMessage(s"Navigating to $url", session) { _.navigate(url) }
          documentId  <- getDocumentNodeId(session)
          result      <- Try { Seq(state.copy(elementStack = Seq(documentId))) }
        } yield result
      case NavigateToDownload(_, _) => Try { throw CrawlerException("Trying to navigate directly to Download in the middle of the script is not possible.") }

    }

  }

  private def elementToSelector(element: HtmlElement): String = {
    (element match {
        case CustomSelector(s) => s
        case Form(_) => "form"
        case Input(_) => "input"
        case Anchor(_) => "a"
        case Div(_) => "div"
        case Span(_) => "span"
        case TableDataCell(_) => "td"
        case TableRow(_) => "tr"
        case Label(_) => "label"
        case Paragraph(_) => "p"
        case AnyElement(_) => "*"


    }) + element.discriminator.map(d => discriminatorXPath(d)).getOrElse("")
  }

  private def discriminatorXPath(discriminator: Discriminator): String = {
    discriminator match {
      case Id(value) => s"[id=$value]"
      case Name(value) => s"[name=$value]"
      case Title(value) => s"[title=$value]"
      case Value(value) => s"[value=$value]"
      case ContainsText(value) => s":contains($value)"
    }
  }

}

object Crawler {
  def defaultLauncher = new Launcher()
  def defaultSessionFactory(launcher: Launcher): SessionFactory = launcher.launch("chromium-browser", defaultArguments)
  val defaultArguments = List("--headless", "--disable-gpu", "--window-size=1280,1024", "--no-sandbox")
}