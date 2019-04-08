package ch.epfl.lara.engine

import ch.epfl.lara.engine.game.environment.{Door, Position, Room}
import ch.epfl.lara.engine.game.items.ItemRegistry


/**
  * @author Louis Vialar
  */
package object game {




  case class LevelMap(objects: ItemRegistry, rooms: RoomRegistry)

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

    def apply(rooms: Seq[Room], doors: Seq[Door]): RoomRegistry = {
      val registry = RoomRegistry(addRooms(Map(), rooms), addDoors(Map(), doors))

      if (!registry.areDoorsValid) {
        val invalidDoors = registry.invalidDoors.map(d => s"(${d.doorType.name} from ${d.left} to ${d.right})").mkString(", \n")
        throw new IllegalArgumentException("some doors go to nowhere: \n" + invalidDoors)
      } else registry
    }
  }

  // TODO: GameState should contain the game map


  case class Scene()



  case class Person()

  // Conditions to open?


  case class Edge(source: Scene, target: Scene, activator: LevelState => Boolean)

  class ScenesGraph(vertices: Seq[Scene], edges: Seq[Edge]) {

    def findNextScene(start: Scene, decisions: LevelState): Option[Scene] = {
      edges find (e => e.source == start && e.activator(decisions)) map (_.target)
    }
  }

  /*
  Kind of stuff:
  - A scene: like a sequence
    contains
      - rooms
      - people
      - objects
      - ...
   */
}
