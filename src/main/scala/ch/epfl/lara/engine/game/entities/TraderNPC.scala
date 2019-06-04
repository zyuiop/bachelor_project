package ch.epfl.lara.engine.game.entities

import java.io.PrintStream

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.environment.{Position, Room}
import ch.epfl.lara.engine.game.items.{InventoryLike, Storable}
import ch.epfl.lara.engine.game.messaging.Message.{RoomMovement, TalkingMessage, TradeRequestResult}
import ch.epfl.lara.engine.game.messaging.Request.InventoryTradeRequest
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler, Request}

/**
  * @author Louis Vialar
  */
class TraderNPC(room: Room, name: String, startInventory: Map[Storable, Int], val prices: Map[Storable, Int])
  extends CharacterState(room, name, startInventory, Map(), new PrintStream(_ => ()))
    with NPC {

  override def currentRoom_=(target: Room): Unit = {
    throw new UnsupportedOperationException("Cannot move a trader!")
  }

  private def sendListOfGoods(target: MessageHandler) = {
    inventory.getContent
      .filterKeys(pickable => prices.contains(pickable))
      .foreach(line => {
        target ! TalkingMessage(this, s"\t${line._2}\t*\t${line._1.displayName}\tfor\t${prices(line._1)} coins each.")
      })
  }

  override def handle(message: Message): Unit = {
    // Delay the response by 1 tick
    GameState.scheduler.runOnce(1)((_, _) => handleMessage(message))
  }

  private def handleMessage(message: Message): Unit = message match {
    case TalkingMessage(sentBy, content) =>
      if (content.toLowerCase().startsWith("buy")) {
        val (pickableName, amt) = InventoryLike.extractItemNameAndQuantity(content.split(" ").drop(1))
        val item = inventory.getItemByName(pickableName)
        val priceOpt = item.toOption.flatMap(prices.get)

        if (priceOpt.isEmpty) {
          sentBy ! TalkingMessage(this, "Sadly I don't sell this kind of stuff here...")
        } else {
          val (pickable, price) = (item.get, priceOpt.get)

          if (inventory.canTake(pickable, amt)) {
            val finalPrice = price * amt

            sentBy ! InventoryTradeRequest(this, sentBy, Map(pickable -> amt), Map(GameState.currency -> finalPrice))
          } else {
            sentBy ! TalkingMessage(this, "I don't have enough of this! Sorry...")
          }
        }
      }

    case RoomMovement(sentBy, true) =>
      if (inventory.nonEmpty) {
        sentBy ! TalkingMessage(this, "Hello! I have a lot of goods to sell! Here is what I have :")
        sendListOfGoods(sentBy)
        // sentBy ! TalkingMessage(this, "I can tell you what I have left if you ask me 'What do you have to sell' or 'What can I buy'!")
        sentBy ! TalkingMessage(this, "If you want to buy me something, just ask me 'buy (amount) (item)'. Enjoy shopping!")
      }
      else {
        sentBy ! TalkingMessage(this, "Hello! Sadly, I have nothing left to sell for now... Come back later!")
      }

    case TradeRequestResult(req, true) =>
      req.target ! TalkingMessage(this, "Thank you! It's always a pleasure to do business with you!")

    case _ =>
      super.handle(message)
  }

  override def handleRequest(request: Request): Unit = request.refuse()
}
