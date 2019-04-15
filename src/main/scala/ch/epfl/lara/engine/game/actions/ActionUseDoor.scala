package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.PlayerState
import ch.epfl.lara.engine.game.environment.Position

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionUseDoor(direction: Option[Position]) extends Action {
  /**
    * Returns the result of executing this action on a given level state
    *
    * @param inState the state of the level at the beggining
    * @param out     a print stream
    * @return the state of the level after executing this action
    */
  override def apply(inState: PlayerState, out: PrintStream): PlayerState = {
    implicit val ps: PrintStream = out
    inState.getDoor(direction.getOrElse(inState.currentPosition)) match {
      case Some(door) =>
        if (door.isOpen(inState)) {
          val (roomId, pos) = door.use(inState.currentRoom)
          val room = inState.map.rooms.getRoom(roomId)

          out.println(room.describe(inState.map))

          inState.copy(currentRoom = room, currentPosition = pos)
        } else {
          out.println(s"The door is locked...")
          inState
        }
      case None =>
        out.println(s"There is no door ${if (direction.isEmpty) "here" else "there"}...")
        inState
    }

  }

}

object ActionUseDoor extends ActionBuilder[ActionUseDoor] {
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
