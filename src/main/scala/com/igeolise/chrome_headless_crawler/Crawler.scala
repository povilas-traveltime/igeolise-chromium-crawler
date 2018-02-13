package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.Crawler.toSelectorString
import crawler.command_parser._
import io.webfolder.cdp.session.{Session, SessionFactory}
import io.webfolder.cdp.session.Extensions._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class CrawlerResult(successes: Seq[File], failures: Seq[Action])
case class CrawlerException(message: String) extends Exception(message)
case class CrawlerCaughtException(message: String, exception: Throwable) extends Exception(s"$message:\n${exception.getMessage}", exception)

class Crawler(chromeSession: ChromeSession, pageWaitTimeout: Int) extends CrawlerHelpers {

  private val log = LoggerFactory.getLogger(classOf[Crawler])

  private case class State(
    elementStack: Seq[Int],
    doneActions: Seq[Action],
    pendingActions: Seq[Action],
    lazyLogger: LazyLog,
    target: File,
    session: Session,
    successes: Seq[File],
    failures: Seq[Seq[Action]],
    unprocessedScripts: Seq[Seq[Action]]
  ) {
    def getStackTop: Try[Int] = Try { elementStack.headOption.getOrElse(throw CrawlerException("Element stack empty")) }
    def appendStack(id: Int): State = this.copy(id +: elementStack)
    def stackTail: State = this.copy(elementStack.tail)
    def moveActionToDone: State = {
      val action = pendingActions.head
      this.copy(doneActions = this.doneActions :+ action, pendingActions = this.pendingActions.tail)
    }
  }

  private def createState(session: Session, actionSequence: Seq[Action], downloadTarget: File): Try[State] = {
    val initState = (documentId: Int) => Try {
      State(
        Seq(documentId), Seq.empty, actionSequence, LazyLog("Crawling " + actionSequence.mkString("|")),
        downloadTarget, session, Seq.empty, Seq.empty, Seq.empty
      )
    }.recover { case e: Throwable =>
        session.close()
        throw e
      }
    for {
      _           <- waitForPage(_.navigate(url), pageWaitTimeout)(session)
      documentId  <- getDocumentNodeId(session)
      state       <- initState(documentId)
    } yield state
  }

  @tailrec
  private def executeScripts(state: State): State = {
    val newState: State = ???

    newState.unprocessedScripts match {
      case head :: tail =>
        val stateForExecution: State = ???
        executeScript(stateForExecution)
      case _ => newState
    }
  }

  def crawl(commands: Seq[Action], downloadLocation: File): CrawlerResult = {
    chromeSession.withSession { session =>

    }




    commands.headOption match {
      case Some(NavigateTo(url)) =>

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

  private def executeIn(state: State, element: HtmlElement): Try[State] = {
    for {
      id      <- getNodeId(state.session, toSelectorString(element))
      result  <- Try { state.appendStack(id) }
    } yield result
  }

  private def click(state: State): Try[State] = {
    for {
      id      <- state.getStackTop
      _       <- waitForPage(_.clickDom(id), pageWaitTimeout) (state.session)
      result  <- Try { state }
    } yield result
  }

  private def clickDownload(state: State): Try[State] = {
    for {
      nodeId    <- state.getStackTop
      download  <- download(state.session, _.clickDom(nodeId), state.target, pageWaitTimeout)
      result    <- ??? //Try { state.copy(results = Some(download)) }
    } yield result
  }

  private def onCurrentPage(state: State): Try[State] = {
    for {
      pageId  <- getDocumentNodeId(state.session)
      result  <- Try { state.copy(elementStack = Seq(pageId)) }
    } yield result
  }

  private def typeIn(state: State, text: String): Try[State] = {
    val rest = (nodeId: Int) => trySessionWithFailureMessage("While typing", state.session) { ses =>
      ses.focus(nodeId)
      ses.sendKeys(text)
      state
    }
    for {
      nodeId <- state.getStackTop
      result <- rest(nodeId)
    } yield result
  }

  private def findContainingInLastResult(state: State, text: String): Try[State] = {
    for {
      nodes   <- trySessionWithFailureMessage("While getting node Ids", session) { _.getNodeIds(s"a:contains('$text')") }
      _       <- Try { nodes.map(n => trySessionWithFailureMessage("While clicking link", state.session) { _.clickDom(n) } ) }
      result  <- Try { state }
    } yield result
  }

  private def navigateTo(state: State, url: String): Try[State] = {
    for {
      _           <- trySessionWithFailureMessage(s"Navigating to $url", session) { _.navigate(url) }
      documentId  <- getDocumentNodeId(state.session)
      result      <- Try { state.copy(elementStack = Seq(documentId)) }
    } yield result
  }

  private def executeAction(action: Action, state: State): Try[State] = {
    import Crawler.toSelectorString

    val session = state.session

    action match {
      case In(element) => executeIn(state, element)
      case Click => click(state)
      case ClickDownload => clickDownload(state)
      case OnCurrentPage => onCurrentPage(state)
      case Up => Try { state.stackTail }
      case TypeIn(text) => typeIn(state, text)
      case FindContainingInLastResult(text) => findContainingInLastResult(state, text)
      case NavigateTo(url) => navigateTo(state, url)

      case ForAllElems(element) =>
        val getNodeIds = (parrentId: Int) => trySessionWithFailureMessage("While getting node Ids", session) { ses =>
          ses.getNodeIds(toSelectorString(element))
        }
        for {
          parentNodeId  <- state.getStackTop
          nodes         <- getNodeIds(parentNodeId)
          attributes    <- Try { nodes.map(n => getNodeAttributes(session, n)).filter(_.isSuccess).map(_.get) }
          elements      <- Try { attributes.map(a => mapAttributesToElement(a)) }
          newStates     <- Try { elements.map(e=> remapScriptWithoutForAll(state, sessionFactory, In(e)).get) }
        } yield newStates





      case NavigateToDownload(_, _) => Try { throw CrawlerException("Trying to navigate directly to Download in the middle of the script is not possible.") }

    }

  }


}

object Crawler {

  def toSelectorString(discriminator: Discriminator): String = {
    discriminator match {
      case Id(value) => s"[id=$value]"
      case Name(value) => s"[name=$value]"
      case Title(value) => s"[title=$value]"
      case Value(value) => s"[value=$value]"
      case ContainsText(value) => s":contains($value)"
    }
  }

  def toSelectorString(element: HtmlElement): String = {
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
}