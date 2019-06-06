package ch.epfl.lara.engine.game.items

import ch.epfl.lara.engine.game.entities.CharacterState

/**
  * An item with which you can interact
  *
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

  /**
    * Returns the actual item, in case of multi-layered items (for example, locks).<br>
    *   This method usually returns `this` but if the item wraps an other item, it will actually return the first item that
    *   doesn't wrap another item
    */
  def underlying: Interactable = this
}
