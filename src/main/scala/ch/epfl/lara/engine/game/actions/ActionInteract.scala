package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.entities.CharacterState

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
        .getInteractableItem(objectName, inState.currentPosition)

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
