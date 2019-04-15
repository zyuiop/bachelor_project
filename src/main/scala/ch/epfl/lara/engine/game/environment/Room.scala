package ch.epfl.lara.engine.game.environment

import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.items.Item
import ch.epfl.lara.engine.game.{LevelMap, MutableInventoryImpl}

/**
  * @author Louis Vialar
  */
case class Room(id: String,
                name: String,
                ambient: String,
                objects: Map[Position, MutableInventoryImpl],
                interactable: Map[String, Map[Position, Item with Interactable]]) {

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
