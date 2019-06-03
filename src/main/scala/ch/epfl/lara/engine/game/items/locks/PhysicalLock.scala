package ch.epfl.lara.engine.game.items.locks

import java.io.PrintStream

import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.items.{ComplexInteractable, Interactable, Item}

import scala.io.AnsiColor

/**
  * A real lock
  *
  * @author Louis Vialar
  */
class PhysicalLock(item: Item with Interactable, name: String, helper: String, code: String, okMessage: String, badMessage: String)
  extends Lock(item) with ComplexInteractable {
  private var _locked = true
  private var _failures = 0

  override def isLocked(state: CharacterState): Boolean = _locked

  override def interactLocked(state: CharacterState): Int = {
    super[ComplexInteractable].interact(state)
  }

  override def interact(state: CharacterState): Int = super[Lock].interact(state)

  override def printClose(implicit ps: PrintStream): Unit = ()

  override def printOpen(implicit ps: PrintStream): Unit = {
    ps.println("The " + item.displayName + " is locked with a " + name + "! " + helper)
    ps.println(AnsiColor.GREEN + "Use the `enter` action to try combinations...")
  }

  handle("try", "type", "enter", "select")((state, args) => {
    if (args.length >= 1) {
      if (code.toLowerCase() == args(0).toLowerCase()) {
        state.ps.println(okMessage)
        _locked = false
        close(state)
        3 + interact(state)
      } else {
        state.ps.println(badMessage)
        _failures += 1
        3
      }
    } else 1
  })

  def failures: Int = _failures
}
