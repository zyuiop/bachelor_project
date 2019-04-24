package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.environment.{Door, Position, Room}
import ch.epfl.lara.engine.game.items.{Inventory, Pickable}
import ch.epfl.lara.engine.game.items.mutable.MutableInventoryImpl
import ch.epfl.lara.engine.game.messaging.Message.{RoomMovement, TalkingMessage}
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class CharacterState(startRoom: Room,
                     startPosition: Position,
                     val name: String = "You",
                     startInventory: Map[Pickable, Int] = Map.empty,
                     startAttributes: Map[String, String] = Map.empty,
                     out: PrintStream = Console.out) extends MessageHandler {

  val inventory: Inventory = new MutableInventoryImpl(startInventory, "inventory") {
    override def printContent(implicit printStream: PrintStream): Unit = {
      printStream.println("Your inventory contains:")
      super.printContent(printStream)
    }
  }
  private val interacts: mutable.ArrayStack[Interactable] = new mutable.ArrayStack[Interactable]()

  var currentRoom: Room = startRoom
  var currentPosition: Position = startPosition
  var attributes: mutable.Map[String, String] = mutable.Map(startAttributes.toList:_*)


  def getDoor(position: environment.Position): Option[Door] = {
    GameState.level.rooms.getDoors(currentRoom).get(position)
  }

  def spawn(): Unit = {
    GameState.registry.addEntity(this)
  }

  def startInteracting(interactWith: Interactable): Unit = interacts.push(interactWith)

  def stopInteracting(): Unit = interacts.pop()

  def currentInteract: Option[Interactable] = if (interacts.nonEmpty) Some(interacts.top) else None

  /**
    * Return the inventory the player interacts with, or, if none, the room inventory
    */
  def currentInventory: Option[Inventory] = {
    val ci = currentInteract
    if (ci.isDefined)
      ci.filter(_.isInstanceOf[Inventory]).map(_.asInstanceOf[Inventory])
    else Some(currentRoom.inventory)
  }

  /**
    * Return the inventory the player interacts with
    */
  def currentOpenInventory: Option[Inventory] =
    currentInteract.filter(_.isInstanceOf[Inventory]).map(_.asInstanceOf[Inventory])


  implicit def ps: PrintStream = out

  def handle(message: Message): Unit = message match {
    case TalkingMessage(sentBy, content) =>
      out.println(Console.CYAN + sentBy + ": " + content + Console.RESET)

    case RoomMovement(sentBy, entering) =>
      out.println(Console.YELLOW + sentBy + " " + (if (entering) "enters" else "leaves") + " the room." + Console.RESET)

  }

    //
}



