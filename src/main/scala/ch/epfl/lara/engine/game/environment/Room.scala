package ch.epfl.lara.engine.game.environment

import java.io.PrintStream

import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.items.{Item, Pickable}
import ch.epfl.lara.engine.game.{Inventory, LevelMap, MutableInventoryImpl}

/**
  * @author Louis Vialar
  */
class Room(val id: String, val name: String, val ambient: String,
           initialItems: Map[Pickable, Int] = Map(),
           interactable: Map[String, Map[Position, Item with Interactable]] = Map()) {

  val inventory: Inventory = new MutableInventoryImpl(initialItems) {
    override def printContent(implicit printStream: PrintStream): Unit = {
      printStream.println("On the floor, you find:")
      super.printContent
    }
  }

  def describe(implicit level: LevelMap): String = {
    def describeDoors: String = {
      level.rooms.getDoors(this).map {
        case (pos, door) =>
          val targetRoom = level.rooms.getRoom(door.getTargetRoom(this)).name
          s"Facing $pos is a ${door.doorType.name} leading to $targetRoom"
      }.mkString("\n")
    }

    s"""You are in: $name.
       |$ambient
       |$describeDoors""".stripMargin
  }

  def getInteractableItem(name: String, position: Position): Option[Item with Interactable] = {
    interactable.get(name.toLowerCase).flatMap(m => {
      if (m.size > 1) m.get(position)
      else if (m.isEmpty) None
      else Some(m.head._2)
    })
  }
}
