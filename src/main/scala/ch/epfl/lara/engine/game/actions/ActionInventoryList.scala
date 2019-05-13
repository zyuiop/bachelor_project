package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.items.Inventory

import scala.util.Try

/**
  * @author Louis Vialar
  */
case object ActionInventoryList extends InventoryAction with ActionBuilder {
  override def apply(v1: CharacterState, inventory: Inventory): Int = {
    inventory.printContent(v1.ps)
    5
  }

  override val triggeringKeywords: Set[String] = Set("list", "search", "probe")

  override def apply(input: Array[String]): Try[Action] = Try {
    this
  }
}
