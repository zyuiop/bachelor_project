package ch.epfl.lara.engine.game.items

import ch.epfl.lara.engine.game.entities.CharacterState

/**
  * @author Louis Vialar
  */
trait Interactable extends Item {
  /**
    * Computes the result of the player interacting with this entity
    *
    * @param state the state of the player interacting
    * @return the time the interaction took
    */
  def interact(state: CharacterState): Int

  def isDoor: Boolean = false
}
