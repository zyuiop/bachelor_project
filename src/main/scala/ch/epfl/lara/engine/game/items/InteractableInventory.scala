package ch.epfl.lara.engine.game.items

import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.entities.Interactable

/**
  * @author Louis Vialar
  */
trait InteractableInventory extends Interactable with Inventory {
  /**
    * Computes the result of the player interacting with this entity
    *
    * @param state the state of the player interacting
    * @return the time the interaction took
    */
  override def interact(state: CharacterState): Int = {
    state.startInteracting(this)
    printOpen(state.ps)
    5
  }
}
