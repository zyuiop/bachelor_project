package ch.epfl.lara.engine.game.entities

import ch.epfl.lara.engine.game.{CharacterState, GameState}
import ch.epfl.lara.engine.game.messaging.Message
import ch.epfl.lara.engine.game.scheduler.Schedulable

/**
  * @author Louis Vialar
  */
class PPC(startState: CharacterState, val states: Set[String], programs: Map[String, String], firstState: String, stateTransitions: Map[String, String], triggers: List[(String, String)]) extends NPC(startState, programs(firstState), triggers) {
  var controlled = false

  def takeControl() = {
    controlled = true
    GameState.registry.removeEntity(this)
  }

  def releaseControl() = {
    controlled = false
    spawn()
  }

  override protected def runNextCommand(tick: Int): Schedulable = {
    if (!controlled) super.runNextCommand(tick)
    else null
  }

  override protected def runTriggers(implicit trigger: Option[Message]): Unit = {
    if (!controlled) super.runTriggers
  }
}

object PPC {
  def apply(startState: CharacterState, program: String, triggers: List[(String, String)]): PPC = {
    new PPC(startState, Set("unique"), Map("unique" -> program), "unique", Map(), triggers)
  }
}