package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.environment.{Door, Position, Room}

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class PlayerState(startRoom: Room,
                  startPosition: Position,
                  val name: String = "You",
                  startInventory: Inventory = Inventory.empty,
                  startAttributes: Map[String, String] = Map.empty,
                  firstParser: ActionParser = ActionParser.DefaultParser,
                  out: PrintStream = Console.out) {

  val inventory: Inventory = new MutableInventoryImpl(startInventory.getContent)
  private val parsers: mutable.ArrayStack[ActionParser] = new mutable.ArrayStack[ActionParser]()

  var currentRoom: Room = startRoom
  var currentPosition: Position = startPosition
  var attributes: mutable.Map[String, String] = mutable.Map(startAttributes.toList:_*)

  // Add first parser
  parsers.push(firstParser)

  def getDoor(position: environment.Position): Option[Door] = {
    GameState.level.rooms.getDoors(currentRoom).get(position)
  }

  def spawn(): Unit = {
    GameState.registry.addEntity(this)
  }

  def addParser(parser: ActionParser): Unit = {
    parsers.push(parser)
  }

  def ps: PrintStream = out

  def currentParser: ActionParser = ActionParser(parsers.top, currentRoom.inventory.actionParser)

  def dequeueParser(): Unit = {
    if (parsers.tail.isEmpty) throw new IllegalStateException("cannot dequeue last parser")
    else parsers.pop()
  }

  def !(message: String): Unit = out.println(Console.CYAN + message + Console.RESET)
}
