package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.entities.ProgrammedNPC
import ch.epfl.lara.engine.game.items.Pickable
import ch.epfl.lara.engine.game.scheduler.Scheduler

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class GameState(val level: LevelMap, val startTime: Int = 0) {
  GameState.instance = Some(this)

  val scheduler: Scheduler = new Scheduler(startTime)
  val attributes: mutable.Map[String, String] = mutable.Map()
  val registry: EntitiesRegistry = new EntitiesRegistry

  def isLevelComplete(playerState: CharacterState): Boolean = false

  def isLevelFailed(playerState: CharacterState): Boolean = false

}

object GameState {
  private var instance: Option[GameState] = None

  val Currency: Pickable = Pickable("coin")

  def get: GameState = instance.get

  def scheduler: Scheduler = get.scheduler

  def attributes: mutable.Map[String, String] = get.attributes

  def level: LevelMap = get.level

  def registry: EntitiesRegistry = get.registry
}