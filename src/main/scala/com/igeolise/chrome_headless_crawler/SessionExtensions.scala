package com.igeolise.chrome_headless_crawler

import java.lang.Math.floor

import com.igeolise.chrome_headless_crawler.model.LogEntry
import io.webfolder.cdp.`type`.constant.MouseButtonType.Left
import io.webfolder.cdp.`type`.constant.MouseEventType.{MousePressed, MouseReleased}
import io.webfolder.cdp.exception.ElementNotFoundException
import io.webfolder.cdp.session.{Mouse, Session}

import scala.collection.JavaConverters._
import scalaz.\/

/**
  * Modified methods of the io.webfolder.cdp.session packge
  */

object SessionExtensions {

  implicit class MouseExt(val mouse: Mouse) extends AnyVal {
    /**
      * Copied from io.webfolder.cdp.session.Mouse
      * Behaviour differs in that this method takes node id directly instead of getting it from the dom by the
      * means of a selector.
      */
    def clickDom(nodeId: Int): LogEntry \/ Session = {
      \/.fromTryCatchNonFatal {
        val dom = mouse.getThis.getCommand.getDOM
        val boxModel = dom.getBoxModel(nodeId, null, null)
        if (boxModel == null)
          throw new ElementNotFoundException(s"NodeId: $nodeId")
        else {
          val content = boxModel.getContent
          if (content == null || content.size() < 2)
            throw new ElementNotFoundException("Could not retrieve content box.")
          else {
            val left = floor(content.get(0))
            val top = floor(content.get(1))
            val input = mouse.getThis.getCommand.getInput
            val clickCount = 1
            input.dispatchMouseEvent(MousePressed, left, top, null, null, Left, clickCount, null, null)
            input.dispatchMouseEvent(MouseReleased, left, top, null, null, Left, clickCount, null, null)
            mouse.getThis
          }
        }
      }.leftMap(e => LogEntry(s"Failed while getting node id's with message: ${e.getMessage}"))
    }
  }

  implicit class SessionExt(val session: Session) extends AnyVal {

    /**
      * Copied from io.webfolder.cdp.session.Selector
      * Behaviour differs that this method return all resolved node ids instead of just the first one.
      */
    def getNodeIds(selector: String): LogEntry \/ List[Int] = {
      \/.fromTryCatchNonFatal {
        val useSizzle = session.useSizzle()
        val trimmedSelector = selector.trim
        val dom = session.getCommand.getDOM
        val useXpath = trimmedSelector.startsWith("/")
        if (useXpath || useSizzle) {
          val objectIds = session.getObjectIds(selector)
          objectIds.asScala.map { id =>
            val nodeId = dom.requestNode(id)
            session.releaseObject(id)
            nodeId.toInt
          }.toList
        } else {
          val documentId = dom.getDocument().getNodeId
          dom.querySelectorAll(documentId, selector).asScala.map(_.toInt).toList
        }
      }.leftMap(e => LogEntry(s"Failed while getting node ids\nException message: ${e.getMessage}"))
    }


  }
}
