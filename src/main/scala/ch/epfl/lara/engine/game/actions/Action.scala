package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.PlayerState

/**
  * @author Louis Vialar
  */
trait Action extends ((PlayerState, PrintStream) => (PlayerState, Int)) {
  /**
    * Returns the result of executing this action on a given level state
    * @param inState the state of the level at the beggining
    * @param out a print stream
    * @return the state of the level after executing this action and the time it took, in seconds
    */
  def execute(inState: PlayerState)(implicit out: PrintStream): (PlayerState, Int) = apply(inState, out)

  def apply(v1: PlayerState, v2: PrintStream): (PlayerState, Int)
}
