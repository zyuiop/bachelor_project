package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.scheduler.Scheduler

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class GameState(val level: LevelMap) {
  GameState.instance = Some(this)

  val scheduler: Scheduler = new Scheduler(0)
  val attributes: mutable.Map[String, String] = mutable.Map()
}

object GameState {
  private var instance: Option[GameState] = None

  def get: GameState = instance.get
}