package ch.epfl.lara.engine.game.decisions

import java.io.PrintStream

import ch.epfl.lara.engine.game.LevelState

/**
  * @author Louis Vialar
  */
trait Action extends ((LevelState, PrintStream) => LevelState) {
  /**
    * Returns the result of executing this action on a given level state
    * @param inState the state of the level at the beggining
    * @param out a print stream
    * @return the state of the level after executing this action
    */
  def execute(inState: LevelState)(implicit out: PrintStream): LevelState = apply(inState, out)

  def apply(v1: LevelState, v2: PrintStream): LevelState
}
