package ch.epfl.lara.engine.game.items.interactables

import ch.epfl.lara.engine.game.characters.CharacterState
import ch.epfl.lara.engine.game.items.{Interactable, Item}
import ch.epfl.lara.engine.game.messaging.Message.SwitchChangeState

/**
  * @author Louis Vialar
  */
class SwitchItem(states: Seq[String], stateTransitions: Map[String, String], val name: String, override val displayName: String, val interactTime: Int = 3, val isInfinite: Boolean = true) extends Item with Interactable {
  private var _currentState = states.head

  def currentState: String = _currentState

  /**
    * Computes the result of the player interacting with this item
    *
    * @param state the state of the player interacting
    * @return the time the interaction took
    */
  override def interact(state: CharacterState): Int = {
    val nextStateIndex = states.indexOf(currentState) + 1
    val nextState = if (nextStateIndex >= states.size) (if (isInfinite) states.head else states.last) else states(nextStateIndex)
    val transition = stateTransitions(nextState)

    state.ps.println(transition)
    state.currentRoom ! SwitchChangeState(name, currentState, nextState)
    this._currentState = nextState
    interactTime
  }
}
