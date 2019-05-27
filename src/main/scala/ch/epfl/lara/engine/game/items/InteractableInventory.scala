package ch.epfl.lara.engine.game.items

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.ActionParser
import ch.epfl.lara.engine.game.entities.CharacterState

/**
  * @author Louis Vialar
  */
trait InteractableInventory extends Interactable with InventoryLike with InventoryInterceptor {
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

  override def printClose(implicit ps: PrintStream): Unit = super[InventoryLike].printClose

  override def updateParser(previousParser: ActionParser): ActionParser =
    parser
      .union(previousParser.alterResult(res =>
        // Close the inventory before
        res.map(action => {
          v1: CharacterState => {
            close(v1)
            action(v1)
          }
        })))

}
