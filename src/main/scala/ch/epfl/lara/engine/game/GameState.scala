package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.entities.{CharacterState, EntitiesRegistry}
import ch.epfl.lara.engine.game.environment.RoomRegistry
import ch.epfl.lara.engine.game.items.Pickable
import ch.epfl.lara.engine.game.scheduler.Scheduler

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class GameState(val level: RoomRegistry, val startTime: Int, val currency: Pickable) {

  if (GameState.instance.nonEmpty) {
    GameState.instance.get.destroy
  }

  private def destroy = ()

  GameState.instance = Some(this)

  val scheduler: Scheduler = new Scheduler(startTime)
  val attributes: mutable.Map[String, String] = mutable.Map()
  val registry: EntitiesRegistry = new EntitiesRegistry

  def isLevelComplete(playerState: CharacterState): Boolean = false

  def isLevelFailed(playerState: CharacterState): Boolean = false

}

object GameState {
  private var instance: Option[GameState] = None

  def Currency: Pickable = get.currency

  def get: GameState = instance.get

  def scheduler: Scheduler = get.scheduler

  def attributes: mutable.Map[String, String] = get.attributes

  def level: RoomRegistry = get.level

  def registry: EntitiesRegistry = get.registry
}