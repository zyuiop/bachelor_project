package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.decisions._
import ch.epfl.lara.engine.game.environment.{Center, Door, Position, Room}
import ch.epfl.lara.engine.game.items.Item

/**
  * @author Louis Vialar
  */
case class LevelState(tree: List[Command],
                      inventory: List[(Object, Int)],
                      currentRoom: Room,
                      currentPosition: Position,
                      currentUsedItem: Option[Item],
                      attributes: Map[String, String],
                      map: LevelMap)(implicit out: PrintStream) {

  def nextState(action: Command): LevelState = action match {
    case MoveCommand(direction: Position) =>
      LevelState(action :: tree, inventory, currentRoom, direction, None, attributes, map)

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


    case TakeDoorCommand(direction: Option[Position]) =>
      takeDoor(direction.getOrElse(currentPosition)) match {
        case Some(door) =>
          if (door.isOpen(this)) {
            val (roomId, pos) = door.use(currentRoom)
            val room = map.rooms.getRoom(roomId)

            out.println(room.describe(map))

            LevelState(action :: tree, inventory, room, pos, None, attributes, map)
          } else {
            out.println(s"The door is locked...")
            this
          }
        case None =>
          out.println(s"There is no door ${if (direction.isEmpty) "here" else "there"}...")
          this
      }


    case InvalidCommand(error: String) =>
      out.println("This command is invalid: " + error)
      this
  }

  private def takeDoor(position: environment.Position) = {
    map.rooms.getDoors(currentRoom).get(position)
  }
}
