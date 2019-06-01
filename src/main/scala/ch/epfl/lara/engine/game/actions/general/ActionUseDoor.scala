package ch.epfl.lara.engine.game.actions.general

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder}
import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.environment.Position
import ch.epfl.lara.engine.game.messaging.Message.RoomMovement

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionUseDoor(direction: Position) extends Action {
  override def apply(inState: CharacterState): Int = {
    inState.getDoor(direction) match {
      case Some(door) =>
        if (door.isOpen(inState)) {
          val (roomId, pos) = door.use(inState.currentRoom)(inState.ps)
          val room = GameState.level.getRoom(roomId)

          inState.changeRoom(room)

          7
        } else {
          inState.ps.println(s"The door is locked...")

          5
        }
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
  override val triggeringKeywords: Set[String] = Set("use", "take", "pass", "climb", "descend", "move", "go","walk")
}
