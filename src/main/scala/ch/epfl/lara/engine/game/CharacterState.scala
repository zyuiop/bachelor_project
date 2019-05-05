package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.environment.{Door, Position, Room}
import ch.epfl.lara.engine.game.items.mutable.MutableInventoryImpl
import ch.epfl.lara.engine.game.items.{Inventory, Pickable}
import ch.epfl.lara.engine.game.messaging.Message.{RoomMovement, SystemMessage, TalkingMessage}
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler, Request}

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class CharacterState(startRoom: Room,
                     startPosition: Position,
                     val name: String,
                     startInventory: Map[Pickable, Int] = Map.empty,
                     startAttributes: Map[String, String] = Map.empty,
                     out: PrintStream = Console.out) extends MessageHandler {

  protected val _inventory: Inventory = new MutableInventoryImpl(startInventory, "inventory") {
    override def printContent(implicit printStream: PrintStream): Unit = {
      printStream.println("Your inventory contains:")
      super.printContent(printStream)
    }
  }

  def inventory: Inventory = _inventory

  private val interacts: mutable.ArrayStack[Interactable] = new mutable.ArrayStack[Interactable]()

  private var _currentRoom: Room = startRoom
  private var _currentPosition: Position = startPosition
  private var _attributes: mutable.Map[String, String] = mutable.Map(startAttributes.toList: _*)

  def currentRoom: Room = _currentRoom

  def currentPosition: Position = _currentPosition

  def attributes: Map[String, String] = _attributes.toMap

  def currentRoom_=(target: Room): Unit = {
    this._currentRoom = target
  }

  def currentPosition_=(target: Position): Unit = {
    this._currentPosition = target
  }

  def changeAttribute(key: String, value: String): Unit = {
    this._attributes.update(key, value)
  }

  def getDoor(position: environment.Position): Option[Door] = {
    GameState.level.rooms.getDoors(_currentRoom).get(position)
  }

  def spawn(): Unit = {
    GameState.registry.addEntity(this)
  }

  def despawn(): Unit = {
    GameState.registry.removeEntity(this)
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
    else Some(_currentRoom.inventory)
  }

  /**
    * Return the inventory the player interacts with
    */
  def currentOpenInventory: Option[Inventory] =
    currentInteract.filter(_.isInstanceOf[Inventory]).map(_.asInstanceOf[Inventory])


  implicit def ps: PrintStream = out

  def handle(message: Message): Unit = message match {
    case TalkingMessage(sentBy, content) =>
      out.println(Console.CYAN + sentBy.name + ": " + content + Console.RESET)

    case SystemMessage(content) =>
      out.println(content)

    case RoomMovement(sentBy, entering) =>
      out.println(Console.YELLOW + sentBy.name + " " + (if (entering) "enters" else "leaves") + " the room." + Console.RESET)

    case r: Request =>
      receivedRequests.put(r.requestId, r)
      handleRequest(r)

    case _ => // unhandled
  }

  def handleRequest(request: Request): Unit = request match {
    case _ =>
      ps.println(s"Received a request from ${request.sentBy.name}:\n$request")
  }

  private val receivedRequests: mutable.Map[Int, Request] = mutable.Map()

  def activeRequests: Iterable[Request] = receivedRequests.values.filter(p => p.sentBy.currentRoom == currentRoom)

  def removeAndGetRequest(id: Int): Option[Request] = {
    if (receivedRequests.contains(id) && receivedRequests(id).sentBy.currentRoom == currentRoom)
      receivedRequests.remove(id)
    else None
  }

  //
}



