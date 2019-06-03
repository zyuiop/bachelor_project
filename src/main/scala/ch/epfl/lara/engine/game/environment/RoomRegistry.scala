package ch.epfl.lara.engine.game.environment

/**
  * @author Louis Vialar
  */
case class RoomRegistry(rooms: Map[String, Room]) {
  def getRoom(id: String): Room = rooms(id)
}

object RoomRegistry {
  def apply(rooms: Seq[Room]): RoomRegistry =
    RoomRegistry(rooms.map(room => room.id -> room).toMap)
}
