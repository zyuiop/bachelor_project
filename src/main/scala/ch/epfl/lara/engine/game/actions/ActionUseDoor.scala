package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.{CharacterState, GameState}
import ch.epfl.lara.engine.game.environment.Position
import ch.epfl.lara.engine.game.messaging.Message.RoomMovement

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionUseDoor(direction: Option[Position]) extends Action {
  override def apply(inState: CharacterState): Int = {
    inState.getDoor(direction.getOrElse(inState.currentPosition)) match {
      case Some(door) =>
        if (door.isOpen(inState)) {
          val (roomId, pos) = door.use(inState.currentRoom)(inState.ps)
          val room = GameState.level.rooms.getRoom(roomId)

          inState.ps.println(room.describe())

          val prev = inState.currentRoom

          room ! RoomMovement(inState, entering = true)

          inState.currentRoom = room

          prev ! RoomMovement(inState, entering = false)

          inState.currentPosition = pos

          7
        } else {
          inState.ps.println(s"The door is locked...")

          5
        }
      case None =>
        inState.ps.println(s"There is no door ${if (direction.isEmpty) "here" else "there"}...")
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

      val direction =
        if (args.nonEmpty) Some(Position.parse(args.mkString("-")))
        else None

      ActionUseDoor(direction)
    }
  }

  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = Set("use", "take", "pass", "climb", "descend", "move", "go","walk")
}
