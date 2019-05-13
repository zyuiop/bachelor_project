package ch.epfl.lara.engine.game.items

import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.messaging.Message.SwitchChangeState

/**
  * An item that simply prints a text when somebody interacts with it.
  *
  * @author Louis Vialar
  */
class DescriptiveItem(override val displayName: String, loreText: String, interactTime: Int = 3) extends Item with Interactable {
  override def interact(state: CharacterState): Int = {
    state.ps.println(loreText)
    interactTime
  }
}
