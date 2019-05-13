package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.entities.{CharacterState, PlayerState}
import ch.epfl.lara.engine.game.items.Inventory
import ch.epfl.lara.engine.game.messaging.Request.InventoryTradeRequest

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionGive(objectName: String, quantity: Int, characterName: String) extends Action {
  override def apply(inState: CharacterState): Int = {
    if (inState.currentOpenInventory.nonEmpty) {
      inState.ps.println("You cannot do that here!")
      return 0
    }

    val characters =
      GameState.registry.getEntities(inState.currentRoom)
        .filter(_.name.toLowerCase.startsWith(characterName.toLowerCase))

    if (characters.isEmpty) {
      inState.ps.println("There is nobody by that name here...")
      0
    } else if (characters.size > 1) {
      inState.ps.println("There are too many characters by that name here... Try to use the full character name.")
      0
    } else {
      val c = characters.head

      if (c == inState || (inState.isInstanceOf[PlayerState] && inState.asInstanceOf[PlayerState].controlled.contains(c))) {
        inState.ps.println("You cannot give items to yourself!")
        return 0
      }

      inState.inventory.getItemByName(objectName).map(item => {
        if (inState.inventory.canTake(item, quantity)) {
          inState.inventory.take(item, quantity)
          c ! InventoryTradeRequest(inState, c, Map((item, quantity)), Map())
        } else {
          inState.ps.println("There are not enough items to send")
        }


      }).recover {
        case e: IllegalArgumentException => inState.ps.println(e.getMessage)
      }

      3
    }

  }
}

object ActionGive extends ActionBuilder {
  override def apply(input: Array[String]): Try[Action] = Try {
    // Split parts
    val toIndex = input.map(_.toLowerCase).indexOf("to")

    if (toIndex == -1) {
      throw new IllegalArgumentException("Missing 'to' separator to indicate target")
    }

    val (left, right) = input splitAt toIndex
    val (item, quantity) = Inventory.extractItemNameAndQuantity(left drop 1)
    val personName = right drop 1 dropWhile (s => s.toLowerCase == "the") mkString " "

    ActionGive(item, quantity, personName)
  }

  override val triggeringKeywords: Set[String] = Set("give", "send")
}