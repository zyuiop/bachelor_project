package ch.epfl.lara.engine.game.environment

import java.io.PrintStream

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.actions.{ActionInterceptor, ActionParser}
import ch.epfl.lara.engine.game.items.interactables.SwitchItem
import ch.epfl.lara.engine.game.items._
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}

/**
  * @author Louis Vialar
  */
class Room(val id: String, val name: String, val ambient: String,
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

  def handle(message: Message): Unit = {
    GameState.registry.getEntities(this).foreach(_ ! message)
  }

  def getInteractableItem(name: String, position: Option[Position] = None): Option[Item with Interactable] = {
    interactable.get(name.toLowerCase).flatMap(m => {
      if (m.size > 1) position.flatMap(p => m.get(p))
      else if (m.isEmpty) None
      else Some(m.head._2)
    })
  }

  def getDoor(position: Position): Option[Item with Interactable] = {
    val doors = interactable.values.flatMap(m => m.get(position)).filter(_.isDoor)
    if (doors.size > 1) None
    else doors.headOption
  }

  def switches: Map[String, String] = interactable.values.flatMap(_.values)
    .filter(_.isInstanceOf[SwitchItem]).map(_.asInstanceOf[SwitchItem])
    .map(switch => switch.name -> switch.currentState)
    .toMap


  override def toString = s"Room($id, $name)"
}
