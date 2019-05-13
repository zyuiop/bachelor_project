package ch.epfl.lara.engine.game.entities

import ch.epfl.lara.engine.game.environment.Room

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class EntitiesRegistry {
  def player: CharacterState = entities.filter(_.isInstanceOf[CharacterState]).head

  val entities: mutable.ArrayBuffer[CharacterState] = mutable.ArrayBuffer()

  def addEntity(entity: CharacterState): Unit = entities += entity

  def getEntities(room: Room): List[CharacterState] = entities.filter(_.currentRoom == room).toList

  def removeEntity(entity: CharacterState): Unit = entities -= entity
}
