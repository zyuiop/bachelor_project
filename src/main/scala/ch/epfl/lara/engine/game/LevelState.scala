package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.decisions._
import ch.epfl.lara.engine.game.environment.{Center, Door, Position, Room}
import ch.epfl.lara.engine.game.items.Item

/**
  * @author Louis Vialar
  */
case class LevelState(inventory: List[(Object, Int)],
                      currentRoom: Room,
                      currentPosition: Position,
                      currentUsedItem: Option[Item],
                      attributes: Map[String, String],
                      map: LevelMap)(implicit out: PrintStream) {

  @deprecated
  def nextState(action: Command): LevelState = action match {
    case MoveCommand(direction: Position) =>
      LevelState(inventory, currentRoom, direction, None, attributes, map)

    case ItemDropCommand(o: Item, quantity: Int) =>
      ???

    case ItemInteractCommand(o: Option[Item]) =>
      ???

    case ItemPickAllCommand =>
      ???

    case ItemPickOneCommand =>
      ???

    case ItemPickCommand(o: Item, quantity: Int) =>
      ???

    case ItemSeekCommand(direction: Option[Position]) =>
      ???


    case InvalidCommand(error: String) =>
      out.println("This command is invalid: " + error)
      this
  }

  def getDoor(position: environment.Position) = {
    map.rooms.getDoors(currentRoom).get(position)
  }
}
