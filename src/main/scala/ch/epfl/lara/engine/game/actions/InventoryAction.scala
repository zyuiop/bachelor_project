package ch.epfl.lara.engine.game.actions
import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.items.Inventory

/**
  * @author Louis Vialar
  */
trait InventoryAction extends Action {
  override def apply(v1: CharacterState): Int = {
    // Try to find the inventory
    val inventory = getInventory(v1)

    if (inventory.isEmpty) {
      v1.ps.println("You cannot do that here!")
      0
    } else this.apply(v1, inventory.get)
  }

  def getInventory(state: CharacterState): Option[Inventory] = state.currentInventory

  def apply(state: CharacterState, inventory: Inventory): Int
}
