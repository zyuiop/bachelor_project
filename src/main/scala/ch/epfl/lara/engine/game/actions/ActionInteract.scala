package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.CharacterState

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionInteract(objectName: String) extends Action {
  override def apply(inState: CharacterState): Int = {
    val item = inState.currentRoom
      .getInteractableItem(objectName, inState.currentPosition)

    if (item.isEmpty) {
      println("there is nothing to interact here...")
      0
    } else {
      item.get.interact(inState)
    }
  }
}

object ActionInteract extends ActionBuilder[ActionInteract] {
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionInteract(input.drop(1).mkString(" ").toLowerCase)
  }

  override val triggeringKeywords: Set[String] = Set("interact", "use", "open")
}
