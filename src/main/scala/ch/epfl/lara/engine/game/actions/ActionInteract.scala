package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.PlayerState
import ch.epfl.lara.engine.game.environment.Position

/**
  * @author Louis Vialar
  */
case class ActionInteract(elem: String) extends Action {
  /**
    * Returns the result of executing this action on a given level state
    *
    * @param inState the state of the level at the beggining
    * @param out     a print stream
    * @return the state of the level after executing this action
    */
  override def apply(inState: PlayerState, out: PrintStream): PlayerState = {
    inState.copy(currentPosition = position)(out)
  }
}

