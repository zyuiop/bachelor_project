package ch.epfl.lara.engine.game.messaging

import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.items.Storable
import ch.epfl.lara.engine.game.messaging.Message.{SystemMessage, TradeRequestResult}
import ch.epfl.lara.engine.game.messaging.Request.InventoryTradeRequest

/**
  * Messages are the way differente characters in the world communicate with each other.
  * @author Louis Vialar
  */
sealed trait Message {
}

sealed trait Request extends Message with MessageHandler {
  val requestId: Int = Request.requestId
  val sentBy: CharacterState

  val target: CharacterState

  def accept(): Unit

  def refuse(): Unit

  override def handle(message: Message): Unit = {
    sentBy ! message
    target ! message
  }
}

object Message {

  case class TalkingMessage(sentBy: CharacterState, content: String) extends Message

  case class SystemMessage(content: String) extends Message

  case class RoomMovement(sentBy: CharacterState, entering: Boolean) extends Message

  case class TradeRequestResult(request: InventoryTradeRequest, success: Boolean) extends Message

  case class TakenControl(sentBy: CharacterState) extends Message
  case class ReleasedControl(sentBy: CharacterState) extends Message

  case class SwitchChangeState(switchId: String, sourceState: String, targetState: String) extends Message
}

object Request {
  private var RequestId = 0

  def requestId: Int = {
    RequestId += 1
    RequestId
  }

  case class InventoryTradeRequest(sentBy: CharacterState, target: CharacterState,
                                   sentItems: Map[Storable, Int],
                                   requestedItems: Map[Storable, Int]
                                  ) extends Request {


    private def buildItemLine(line: (Storable, Int)): String =
      line._2 + "\t*\t" + line._1.displayName

    private def buildItemList(list: Map[Storable, Int]): String =
      if (list.size == 1) buildItemLine(list.head)
      else "\n\t" + list.map(buildItemLine).mkString("\n\t")

    override def toString: String = {
      val str = if (requestedItems.nonEmpty) {
        "Please give " + buildItemList(requestedItems)
      } else ""

      if (sentItems.nonEmpty) {
        (if (str.nonEmpty) str + "\nin exchange for "
        else "Please receive ") + buildItemList(sentItems)
      } else str
    }

    def isAcceptable: Boolean = requestedItems.forall(item => target.inventory.canTake(item._1, item._2))

    private def fail() = this ! TradeRequestResult(this, false)

    private def succeed() = this ! TradeRequestResult(this, true)

    override def accept(): Unit = {
      val fullfillable = sentItems.forall(item => sentBy.inventory.canTake(item._1, item._2))

      if (!isAcceptable) {
        target ! SystemMessage("You cannot accept this trade: you don't have enough items.")
        fail()
      } else if (!fullfillable) {
        sentBy ! SystemMessage(target.name + " accepted the trade, but you could not fulfill it as you don't have the offered items anymore.")
        target ! SystemMessage(sentBy.name + " can't offer the advertised items...")
        fail()
      } else {
        sentBy ! SystemMessage(target.name + " accepted the trade!")
        target ! SystemMessage("Trade accepted!")

        sentItems.foreach(item => {
          sentBy.inventory.transferTo(target.inventory, item._1, item._2)
          val line = buildItemLine(item)

          sentBy ! SystemMessage(target.name + " -> " + line)
          target ! SystemMessage(sentBy.name + " <- " + line)
        })

        requestedItems.foreach(item => {
          target.inventory.transferTo(sentBy.inventory, item._1, item._2)

          val line = buildItemLine(item)

          sentBy ! SystemMessage(target.name + " <- " + line)
          target ! SystemMessage(sentBy.name + " -> " + line)
        })

        succeed()
      }
    }

    override def refuse(): Unit = {
      sentBy ! SystemMessage(target.name + " refused the trade...")
      target ! SystemMessage("Trade refused!")
      fail()
    }
  }

}