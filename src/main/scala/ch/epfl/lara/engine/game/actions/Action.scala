package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.entities.CharacterState

/**
  * @author Louis Vialar
  */
trait Action extends (CharacterState => Int) {
  /**
    * Returns the result of executing this action on a given level state
    *
    * @param inState the state of the level at the beggining
    * @return the time it took to execute this action, in seconds
    */
  def apply(inState: CharacterState): Int
}
