package ch.epfl.lara.engine.game.environment

import ch.epfl.lara.engine.game.{ImmutableInventoryImpl, LevelMap, LevelState}
import ch.epfl.lara.engine.game.decisions.Command

/**
  * @author Louis Vialar
  */
case class Room(id: String,
                name: String,
                ambient: String,
                objects: Map[Position, ImmutableInventoryImpl]) {

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

  private def newInventory(decision: Command, inventory: List[(Object, Int)]) = ???
}
