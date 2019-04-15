package ch.epfl.lara.engine.game.entities

import java.io.PrintStream

import ch.epfl.lara.engine.game.{GameState, PlayerState}

/**
  * @author Louis Vialar
  */
trait Interactable {
  /**
    * Computes the result of the player interacting with this entity
    * @param state the source state of the level
    * @return the new state of the scene, as well as the updated version of this interactable
    */
  def interact(state: PlayerState)(implicit out: PrintStream): PlayerState
}
