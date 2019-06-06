package ch.epfl.lara.engine.game.environment

import java.io.PrintStream

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.actions.{ActionInterceptor, ActionParser}
import ch.epfl.lara.engine.game.items.interactables.{DoorItem, SwitchItem}
import ch.epfl.lara.engine.game.items._
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}

/**
  * @author Louis Vialar
  */
class Room(val id: String, val name: String, val ambient: String, val image: Option[String],
           initialItems: Map[Storable, Int] = Map(),
           interactable: Map[String, Map[Position, Item with Interactable]] = Map()) extends MessageHandler with ActionInterceptor {

  val inventory: InventoryLike with InventoryInterceptor = new Inventory(initialItems, "floor") with InventoryInterceptor {
    override def printContent(implicit printStream: PrintStream): Unit = {
      printStream.println("On the floor, you find:")
      super.printContent
    }

    override def printEmpty(implicit ps: PrintStream): Unit = ps.println("\tNothing. There is nothing on the floor.")
  }

  override def updateParser(previousParser: ActionParser): ActionParser = inventory.updateParser(previousParser)

  /**
    * Returns a string that describes the room and its content
    */
  def describe(): String = {
    def describeInteracts: String = {
      if (interactable.nonEmpty) {
        interactable.flatMap {
          case (_, items) =>
            items.map {
              case (direction, item) => s"At the $direction there is ${item.describe}"
            }
        }.mkString("\n")
      } else ""
    }

    s"""You are in: $name.
       |$ambient
       |$describeInteracts""".stripMargin
  }

  /**
    * Handles a message, forwarding it to all the characters in the room
    * @param message the message to handle
    */
  def handle(message: Message): Unit = {
    GameState.registry.getCharacters(this).foreach(_ ! message)
  }

  /**
    * Get an interactable by its name
    * @param name the name of the item
    * @return
    */
  def getInteractableItem(name: String): Option[Interactable] = {
    interactable.get(name.toLowerCase).flatMap(m => {
      if (m.size > 1) None
      else if (m.isEmpty) None
      else Some(m.head._2)
    })
  }

  /**
    * Get a door at a given position
    * @param position the position at which the door should be
    * @return
    */
  def getDoor(position: Position): Option[Interactable] = {
    val doors = interactable.values.flatMap(m => m.get(position)).filter(_.underlying.isInstanceOf[DoorItem])
    if (doors.size > 1) None
    else doors.headOption
  }

  /**
    * Get the values of all the switches in the room
    * @return
    */
  def switches: Map[String, String] = interactable.values.flatMap(_.values)
    .filter(_.underlying.isInstanceOf[SwitchItem]).map(_.underlying.asInstanceOf[SwitchItem])
    .map(switch => switch.name -> switch.currentState)
    .toMap


  override def toString = s"Room($id, $name)"
}
