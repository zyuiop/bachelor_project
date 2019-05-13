package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.control.runner.ConditionExecutionContext
import ch.epfl.lara.engine.game.data.LevelDescriptor
import ch.epfl.lara.engine.game.entities.{CharacterState, EntitiesRegistry}
import ch.epfl.lara.engine.game.environment.RoomRegistry
import ch.epfl.lara.engine.game.items.Pickable
import ch.epfl.lara.engine.game.scheduler.Scheduler

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class GameState(val level: RoomRegistry, val startTime: Int, val currency: Pickable, val levelData: LevelDescriptor,
                val levelSuccess: ConditionExecutionContext, val levelFailure: ConditionExecutionContext) {

  if (GameState.instance.nonEmpty) {
    GameState.instance.get.destroy
  }

  private def destroy = ()

  GameState.instance = Some(this)

  val scheduler: Scheduler = new Scheduler(startTime)
  val attributes: mutable.Map[String, String] = mutable.Map()
  val registry: EntitiesRegistry = new EntitiesRegistry

  def isLevelComplete: Boolean = levelSuccess.checkCondition()

  def isLevelFailed: Boolean = levelFailure.checkCondition()

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