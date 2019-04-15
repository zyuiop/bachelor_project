package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.environment.{Position, Room}
import ch.epfl.lara.engine.game.items.Item
import ch.epfl.lara.engine.game.scheduler.Scheduler

/**
  * @author Louis Vialar
  */
case class PlayerState(inventory: Inventory,
                       currentRoom: Room,
                       currentPosition: Position,
                       currentUsedItem: Option[Item],
                       attributes: Map[String, String],
                       map: LevelMap,
                       commandParsers: List[ActionParser]
                     )
                      (implicit out: PrintStream) {

  def getDoor(position: environment.Position) = {
    map.rooms.getDoors(currentRoom).get(position)
  }

  def addParser(parser: ActionParser): PlayerState = {
    copy(commandParsers = parser :: commandParsers)
  }

  def currentParser: ActionParser = ActionParser(commandParsers.head, currentRoom.inventory.actionParser)

  def dequeueParser(): PlayerState = {
    if (commandParsers.tail.isEmpty) throw new IllegalStateException("cannot dequeue last parser")
    else copy(commandParsers = commandParsers.tail)
  }
}
