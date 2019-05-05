package ch.epfl.lara.engine.game.entities

import ch.epfl.lara.engine.game.{CharacterState, GameState}
import ch.epfl.lara.engine.game.messaging.Message
import ch.epfl.lara.engine.game.scheduler.Schedulable

/**
  * @author Louis Vialar
  */
class PPC(startState: CharacterState, val states: Set[String], program: String, firstState: String) extends ProgrammedNPC(startState, program) {
  var controlled = false

  // TODO: setter command in the program? (or just parse conditions)

  def takeControl(): Unit = {
    controlled = true
    despawn()
  }

  def releaseControl(): Unit = {
    controlled = false
    spawn()
  }
}

object PPC {
  def apply(startState: CharacterState, program: String): PPC = {
    new PPC(startState, Set("unique"), program, "unique")
  }
}