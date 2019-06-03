package ch.epfl.lara.engine.game.items.locks

import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.items.{Interactable, Item}

/**
  * A wrapper that makes an item accessible only under a specific condition, depending on the implementation
  * @author Louis Vialar
  */
abstract class Lock(val locked: Item with Interactable) extends Item with Interactable {
  override val displayName: String = locked.displayName

  override def describe: String = locked.describe

  override def isDoor: Boolean = locked.isDoor

  override def interact(state: CharacterState): Int = {
    if (isLocked(state)) interactLocked(state)
    else interactUnlocked(state)
  }

  /**
    * The interaction ran when the lock is still locked
    * @param state the character accessing the item
    * @return the time the interaction takes
    */
  abstract def interactLocked(state: CharacterState): Int

  /**
    * The interaction ran when the lock is unlocked
    * @param state the character accessing the item
    * @return the time the interaction takes
    */
  def interactUnlocked(state: CharacterState): Int = locked.interact(state)

  /**
    * Checks if the lock is still locked
    * @param state the character accessing the item
    * @return the time the interaction takes
    */
  abstract def isLocked(state: CharacterState): Boolean
}
