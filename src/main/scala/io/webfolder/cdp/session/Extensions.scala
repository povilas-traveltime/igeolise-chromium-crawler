package io.webfolder.cdp.session

import java.lang.Math.floor

import io.webfolder.cdp.`type`.constant.MouseButtonType.Left
import io.webfolder.cdp.`type`.constant.MouseEventType.{MousePressed, MouseReleased}
import io.webfolder.cdp.exception.CdpException

import scala.collection.JavaConverters._

object Extensions {
  implicit class MouseExt(val mouse: Mouse) extends AnyVal {
    def clickDom(nodeId: Int): Session = {
      val dom = mouse.getThis.getCommand.getDOM
      val boxModel = dom.getBoxModel(nodeId, null, null)
      if (boxModel == null) ???
      else {
        val content = boxModel.getContent
        if (content == null ||
          content.size() < 2) ???
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
    }
  }

  implicit class SessionExt(val session: Session) extends AnyVal {

    def getNodeIds(selector: String): Seq[Int] = {
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
        }
      } else {
        val documentId = dom.getDocument().getNodeId
        dom.querySelectorAll(documentId, selector).asScala.map(_.toInt)
      }
    }

    def focus(id: Int): Session = {
      session.getCommand.getDOM.focus(id, null, null)
      session
    }
  }
}
