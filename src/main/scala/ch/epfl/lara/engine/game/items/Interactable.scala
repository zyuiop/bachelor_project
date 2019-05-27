package ch.epfl.lara.engine.game.items

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.ActionInterceptor
import ch.epfl.lara.engine.game.entities.CharacterState

/**
  * @author Louis Vialar
  */
trait Interactable extends ActionInterceptor {
  /**
    * Computes the result of the player interacting with this entity
    *
    * @param state the state of the player interacting
    * @return the time the interaction took
    */
  def interact(state: CharacterState): Int

  def printClose(implicit ps: PrintStream): Unit = ()

  protected def close(state: CharacterState): Int = {
    printClose(state.ps)
    state.stopInteracting()
    3
  }

  handle("close", "quit", "exit") { (state, _) =>
      close(state)
  }
}
