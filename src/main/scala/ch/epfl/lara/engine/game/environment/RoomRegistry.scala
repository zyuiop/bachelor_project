package ch.epfl.lara.engine.game.environment

/**
  * @author Louis Vialar
  */
case class RoomRegistry(rooms: Map[String, Room], doors: Map[String, List[Door]]) {
  def addRooms(newRooms: Room*): RoomRegistry = RoomRegistry(RoomRegistry.addRooms(rooms, newRooms), doors)

  def addDoors(newDoors: Door*) =
    RoomRegistry(rooms, RoomRegistry.addDoors(doors, newDoors))

  def getDoors(room: Room): Map[Position, Door] =
    doors.getOrElse(room.id, Nil).map(door => (door.getPosition(room), door)).toMap

  def getRoom(id: String): Room = rooms(id)

  private lazy val areDoorsValid: Boolean = invalidDoors.isEmpty

  private lazy val invalidDoors: Iterable[Door] = {
    doors.values.flatten.filterNot(d => rooms.contains(d.left) && rooms.contains(d.right))
  }
}

object RoomRegistry {
  private def addRooms(rooms: Map[String, Room], newRooms: Seq[Room]): Map[String, Room] = {
    rooms ++ newRooms.map(room => room.id -> room)
  }

  private def addDoors(doors: Map[String, List[Door]], newDoors: Seq[Door]): Map[String, List[Door]] = {
    if (newDoors.isEmpty) doors
    else {
      val door = newDoors.head
      addDoors(doors + (door.left -> (door :: doors.getOrElse(door.left, Nil))) + (door.right -> (door :: doors.getOrElse(door.right, Nil))), newDoors.tail)
    }
  }

  def apply(rooms: Seq[Room]): RoomRegistry = {
    val registry = RoomRegistry(addRooms(Map(), rooms), Map())

    if (!registry.areDoorsValid) {
      val invalidDoors = registry.invalidDoors.map(d => s"(${d.doorType.name} from ${d.left} to ${d.right})").mkString(", \n")
      throw new IllegalArgumentException("some doors go to nowhere: \n" + invalidDoors)
    } else registry
  }
}
