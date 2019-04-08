package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.decisions._
import ch.epfl.lara.engine.game.environment.{Center, Door, Position, Room}
import ch.epfl.lara.engine.game.items.Item

/**
  * @author Louis Vialar
  */
// TODO: rename to scenestate

case class LevelState(tree: List[Command],
                      inventory: List[(Object, Int)],
                      currentRoom: Room,
                      currentPosition: Position,
                      currentUsedItem: Option[Item],
                      attributes: Map[String, String],
                      map: SceneMap)(implicit out: PrintStream) {

  def nextState(action: Command): LevelState = action match {
    case MoveCommand(direction: Position) =>
      LevelState(action :: tree, inventory, currentRoom, direction, None, attributes, map)

    case ItemDropCommand(o: Item, quantity: Int) =>


    case ItemInteractCommand(o: Option[Item]) =>

    case ItemPickAllCommand =>

    case ItemPickOneCommand =>

    case ItemPickCommand(o: Item, quantity: Int) =>

    case ItemSeekCommand(direction: Option[Position]) =>


    case TakeDoorCommand(direction: Option[Position]) =>
      currentRoom.takeDoor(direction.getOrElse(currentPosition)) match {
        case Some(door) =>
          if (door.isOpen(this))
            LevelState(action :: tree, inventory, door.use(currentRoom), Center, None, attributes, map)
          else {
            out.println(s"The door is locked...")
            this
          }
        case None =>
          out.println(s"There is no door ${if (direction.isEmpty) "here" else "there"}...")
          this
      }


    case InvalidCommand(error: String) =>
  }
}
