package ch.epfl.lara.engine.game.items
import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.messaging.Message.SwitchChangeState

/**
  * @author Louis Vialar
  */
class Switch(states: Seq[String], stateTransitions: Map[String, String], val name: String, override val displayName: String, val interactTime: Int = 3) extends Item with Interactable {
  private var _currentState = states.head

  def currentState: String = _currentState

  /**
    * Computes the result of the player interacting with this entity
    *
    * @param state the state of the player interacting
    * @return the time the interaction took
    */
  override def interact(state: CharacterState): Int = {
    val transition = stateTransitions(currentState)
    val nextStateIndex = states.indexOf(currentState) + 1
    val nextState = if (nextStateIndex >= states.size) states.head else states(nextStateIndex)

    state.ps.println(transition)
    state.currentRoom ! SwitchChangeState(name, currentState, nextState)
    this._currentState = nextState
    interactTime
  }
}
