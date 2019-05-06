package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.items.Inventory

import scala.util.Try

/**
  * @author Louis Vialar
  */

case class ActionInventoryMove(action: String, objectName: String, quantity: Int, moveFromSelf: Boolean) extends InventoryAction {
  override def apply(state: CharacterState, inventory: Inventory): Int = {
    val (source, target) = if (moveFromSelf) (state.inventory, inventory) else (inventory, state.inventory)
    val (verb, dirWord) = if (moveFromSelf) ("dropped", "from") else ("took", "into")

    source.getItemByName(objectName).flatMap(item => {
      Try {
        val (succ, _, _) = source.transferTo(target, item, quantity)

        if (succ) {
          state.ps.println(s"You $verb $quantity * ${item.displayName} $dirWord your inventory.")
          5
        } else {
          state.ps.println("There are not enough items to " + action)
          3
        }
      }
    }).recover({
      case ex: IllegalArgumentException =>
        state.ps.println(ex.getMessage)
        0
    }).get
  }
}

object ActionInventoryMove extends ActionBuilder {
  private val dropWords = Set("drop")
  private val takeWords = Set("take", "pick")

  override def apply(input: Array[String]): Try[Action] = Try {
    val (item, quantity) = Inventory.extractItemNameAndQuantity(input drop 1)

    ActionInventoryMove(input.head, item, quantity, dropWords(input.head))
  }

  override val triggeringKeywords: Set[String] = dropWords union takeWords
}

