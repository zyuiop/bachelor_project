package ch.epfl.lara.engine.game.actions.general

import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder}
import ch.epfl.lara.engine.game.characters.CharacterState
import ch.epfl.lara.engine.game.items.Interactable

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionInteract(objectName: String) extends Action {
  override def apply(inState: CharacterState): Int = {
    // Special: open own inventory
    if (objectName.toLowerCase == "inventory") {
      // Just list content, as all other commands are implicitly related to current own inventory
      inState.inventory.printContent(inState.ps)
      15
    } else {
      val item = inState.currentRoom
        .getInteractableItem(objectName)
        .orElse(
          inState.inventory.getItemByName(objectName.toLowerCase).toOption.flatMap {
            case i: Interactable => Some(i)
            case _ =>
              inState.ps.println("item is not an interactable")
              None
          }
        )

      if (item.isEmpty) {
        inState.ps.println("there is nothing to interact here...")
        0
      } else {
        item.get.interact(inState)
      }
    }
  }
}

object ActionInteract extends ActionBuilder {
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionInteract(input.drop(1).mkString(" ").toLowerCase)
  }

  override val triggeringKeywords: Set[String] = Set("interact", "use", "open")
}
