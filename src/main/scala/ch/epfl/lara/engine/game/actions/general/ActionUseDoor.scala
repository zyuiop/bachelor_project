package ch.epfl.lara.engine.game.actions.general

import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder}
import ch.epfl.lara.engine.game.characters.CharacterState
import ch.epfl.lara.engine.game.environment.Position

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionUseDoor(direction: Position) extends Action {
  override def apply(inState: CharacterState): Int = {
    val room = inState.currentRoom

    room.getDoor(direction) match {
      case Some(door) =>
        door.interact(inState)

        5
      case None =>
        inState.ps.println(s"There is no door here...")
        0
    }
  }

}

object ActionUseDoor extends ActionBuilder {
  /**
    * Build the action from the complete user input
    *
    * @param input the user input, split by spaces
    * @return an optional action
    */
  override def apply(input: Array[String]): Try[Action] = {
    Try {
      val args = if (input.tail.nonEmpty && input.tail.head.toLowerCase == "door") input.tail.tail else input.tail

      val direction = Position.parse(args.mkString("-"))

      ActionUseDoor(direction)
    }
  }

  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = Set("use", "take", "pass", "climb", "descend", "move", "go", "walk")
}
