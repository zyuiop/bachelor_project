package ch.epfl.lara.engine.game.items

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.{ActionInterceptor, ActionParser}
import ch.epfl.lara.engine.game.characters.CharacterState

/**
  * An item with which you interact during a long time, using multiple commands
  *
  * @author Louis Vialar
  */
trait ComplexInteractable extends Interactable with ActionInterceptor {
  def printClose(implicit ps: PrintStream): Unit

  def printOpen(implicit ps: PrintStream): Unit

  override def interact(state: CharacterState): Int = {
    state.startInteracting(this)
    printOpen(state.ps)
    5
  }

  protected def close(state: CharacterState): Int = {
    printClose(state.ps)
    state.stopInteracting()
    3
  }

  handle("close", "quit", "exit") { (state, _) =>
    close(state)
  }

  override def updateParser(previousParser: ActionParser): ActionParser =
    parser
      .union(previousParser.alterResult(res =>
        // Close the interact before doing something else
        res.map(action => {
          v1: CharacterState => {
            close(v1)
            action(v1)
          }
        })))
}
