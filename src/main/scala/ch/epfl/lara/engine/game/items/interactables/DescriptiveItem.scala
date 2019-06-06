package ch.epfl.lara.engine.game.items.interactables

import ch.epfl.lara.engine.game.characters.CharacterState
import ch.epfl.lara.engine.game.items.{Interactable, Item, Storable}

/**
  * An item that simply prints a text when somebody interacts with it.
  *
  * @author Louis Vialar
  */
class DescriptiveItem(override val displayName: String, loreText: String, interactTime: Int = 3) extends Interactable with Storable {
  override def interact(state: CharacterState): Int = {
    state.ps.println(loreText)
    interactTime
  }
}
