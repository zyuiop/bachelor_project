package ch.epfl.lara.engine.game.entities

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.{ActionInterceptor, ActionParser}
import ch.epfl.lara.engine.game.environment.{Position, Room}
import ch.epfl.lara.engine.game.items.{ComplexInteractable, Interactable, Inventory, InventoryLike, Item, Storable}
import ch.epfl.lara.engine.game.messaging.Message.{RoomMovement, SystemMessage, TalkingMessage}
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler, Request}
import ch.epfl.lara.engine.game.{GameState, environment}

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class CharacterState(startRoom: Room,
                     val name: String,
                     startInventory: Map[Storable, Int] = Map.empty,
                     startAttributes: Map[String, String] = Map.empty,
                     out: PrintStream = Console.out) extends MessageHandler with ActionInterceptor {

  protected val _inventory: InventoryLike = new Inventory(startInventory, "inventory") {
    override def printContent(implicit printStream: PrintStream): Unit = {
      printStream.println("Your inventory contains:")
      super.printContent(printStream)
    }
  }

  def inventory: InventoryLike = _inventory

  private val interacts: mutable.ArrayStack[ComplexInteractable] = new mutable.ArrayStack[ComplexInteractable]()

  private var _currentRoom: Room = startRoom
  private var _attributes: mutable.Map[String, String] = mutable.Map(startAttributes.toList: _*)

  def currentRoom: Room = _currentRoom

  def changeRoom(room: Room) = {
    ps.println(room.describe())

    val prev = currentRoom

    room ! RoomMovement(this, entering = true)

    currentRoom = room

    prev ! RoomMovement(this, entering = false)
  }

  def attributes: Map[String, String] = _attributes.toMap

  def currentRoom_=(target: Room): Unit = {
    this._currentRoom = target
  }

  def changeAttribute(key: String, value: String): Unit = {
    this._attributes.update(key, value)
  }

  def spawn(): Unit = {
    GameState.registry.addEntity(this)
  }

  def despawn(): Unit = {
    GameState.registry.removeEntity(this)
  }

  def startInteracting(interactWith: ComplexInteractable): Unit = interacts.push(interactWith)

  def stopInteracting(): Unit = interacts.pop()

  def currentInteract: Option[ComplexInteractable] = if (interacts.nonEmpty) Some(interacts.top) else None

  implicit def ps: PrintStream = out

  def handle(message: Message): Unit = message match {
    case TalkingMessage(sentBy, content) =>
      out.println(Console.CYAN + sentBy.name + ": " + content + Console.RESET)

    case SystemMessage(content) =>
      out.println(content)

    case RoomMovement(sentBy, entering) =>
      out.println((if (entering) Console.GREEN else Console.RED) + sentBy.name + " " + (if (entering) "enters" else "leaves") + " the room." + Console.RESET)

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

  override def updateParser(previousParser: ActionParser): ActionParser = {
    val parser = currentRoom.updateParser(previousParser)

    currentInteract.foldRight(parser)((i, p) => i.updateParser(p))
  }
}



