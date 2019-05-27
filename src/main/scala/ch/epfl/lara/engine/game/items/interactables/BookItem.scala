package ch.epfl.lara.engine.game.items.interactables

import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.items.{Interactable, Pickable}

/**
  * @author Louis Vialar
  */
class BookItem extends Pickable with Interactable {
  /**
    * Computes the result of the player interacting with this entity
    *
    * @param state the state of the player interacting
    * @return the time the interaction took
    */
  override def interact(state: CharacterState): Int = {
    0
  }

  /**
    * The name under which this item can be referenced from the command line
    */
  override val displayName: String = "Book"
}
