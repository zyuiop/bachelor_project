package ch.epfl.lara.engine.game.items.locks

import ch.epfl.lara.engine.game.control.ActionCompiler
import ch.epfl.lara.engine.game.control.runner.ConditionExecutionContext
import ch.epfl.lara.engine.game.characters.CharacterState
import ch.epfl.lara.engine.game.items.{Interactable, Item}

/**
  * A lock using a condition to automatically grant or restrict access to the underlying object
  * @author Louis Vialar
  */
class InvisibleLock(item: Item with Interactable, message: String, condition: String) extends Lock(item) {
  private val _condition = new ConditionExecutionContext(ActionCompiler.compileValue("lock of " + item.displayName, condition))

  /**
    * Checks if the lock is still locked
    *
    * @param state the character accessing the item
    * @return the time the interaction takes
    */
  override def isLocked(state: CharacterState): Boolean = !_condition.checkCondition(_condition.characterEnv(state))

  /**
    * The interaction ran when the lock is still locked
    *
    * @param state the character accessing the item
    * @return the time the interaction takes
    */
  override def interactLocked(state: CharacterState): Int = {
    state.ps.println(message)
    5
  }
}
