package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.decisions._
import ch.epfl.lara.engine.game.environment.{Position, Room}
import ch.epfl.lara.engine.game.items.Item
import ch.epfl.lara.engine.game.scheduler.Scheduler

/**
  * @author Louis Vialar
  */
case class LevelState(inventory: Inventory,
                      currentRoom: Room,
                      currentPosition: Position,
                      currentUsedItem: Option[Item],
                      attributes: Map[String, String],
                      map: LevelMap,
                      commandParsers: List[ActionParser],
                      scheduler: Scheduler = Scheduler(0, Nil)
                     )
                     (implicit out: PrintStream) {

  def getDoor(position: environment.Position) = {
    map.rooms.getDoors(currentRoom).get(position)
  }

  def addParser(parser: ActionParser): LevelState = {
    copy(commandParsers = parser :: commandParsers)
  }

  def currentParser: ActionParser = commandParsers.head

  def dequeueParser(): LevelState = {
    if (commandParsers.tail.isEmpty) throw new IllegalStateException("cannot dequeue last parser")
    else copy(commandParsers = commandParsers.tail)
  }
}
