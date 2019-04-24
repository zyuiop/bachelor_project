package ch.epfl.lara.engine.game.environment

import java.io.PrintStream

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.items.mutable.MutableInventoryImpl
import ch.epfl.lara.engine.game.items.{Inventory, Item, Pickable}
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}

/**
  * @author Louis Vialar
  */
class Room(val id: String, val name: String, val ambient: String,
           initialItems: Map[Pickable, Int] = Map(),
           interactable: Map[String, Map[Position, Item with Interactable]] = Map()) extends MessageHandler  {

  val inventory: Inventory = new MutableInventoryImpl(initialItems, "floor") {
    override def printContent(implicit printStream: PrintStream): Unit = {
      printStream.println("On the floor, you find:")
      super.printContent
    }

    override def printEmpty(implicit ps: PrintStream): Unit = ps.println("\tNothing. There is nothing on the floor.")
  }

  def describe(): String = {
    def describeDoors: String = {
      GameState.level.rooms.getDoors(this).map {
        case (pos, door) =>
          val targetRoom = GameState.level.rooms.getRoom(door.getTargetRoom(this)).name
          s"At the $pos there is a ${door.doorType.name} leading to $targetRoom"
      }.mkString("\n")
    }

    def describeInteracts: String = {
      if (interactable.nonEmpty) {
        "\n" + interactable.flatMap {
          case (name, items) =>
            items.map {
              case (direction, _) => s"At the $direction there is a $name"
            }
        }.mkString("\n")
      } else ""
    }

    s"""You are in: $name.
       |$ambient
       |$describeDoors $describeInteracts""".stripMargin
  }

  def handle(message: Message): Unit = GameState.registry.getEntities(this).foreach(_ ! message)

  def getInteractableItem(name: String, position: Position): Option[Item with Interactable] = {
    interactable.get(name.toLowerCase).flatMap(m => {
      if (m.size > 1) m.get(position)
      else if (m.isEmpty) None
      else Some(m.head._2)
    })
  }
}
