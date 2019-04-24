package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.items.Inventory

import scala.util.Try

/**
  * @author Louis Vialar
  */
case object ActionInventoryClose extends InventoryAction with ActionBuilder {
  override def apply(v1: CharacterState, inventory: Inventory): Int = {
    inventory.printClose(v1.ps)
    v1.stopInteracting()
    3
  }

  override def getInventory(state: CharacterState): Option[Inventory] = state.currentOpenInventory

  override val triggeringKeywords: Set[String] = Set("leave", "quit", "exit", "back", "go", "close")

  override def apply(input: Array[String]): Try[Action] = Try {
    this
  }
}
