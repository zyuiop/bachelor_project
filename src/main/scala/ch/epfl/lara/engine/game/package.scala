package ch.epfl.lara.engine

import ch.epfl.lara.engine.game.environment.RoomRegistry
import ch.epfl.lara.engine.game.items.ItemRegistry


/**
  * @author Louis Vialar
  */
package object game {

  case class LevelMap(objects: ItemRegistry, rooms: RoomRegistry) {

    val entities: EntitiesRegistry = new EntitiesRegistry
  }

}
