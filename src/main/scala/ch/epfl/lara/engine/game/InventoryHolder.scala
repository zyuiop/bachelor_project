package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.items.Item

/**
  * Represents a wrapper to an inventory, namely a map from items to their owned amount
  *
  * @author Louis Vialar
  */
case class InventoryHolder(inventory: Map[Item, Int]) {
  def take(o: Item, quantity: Int): InventoryHolder = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (inventory.getOrElse(o, 0) < quantity)
      throw new IllegalStateException("insufficient quantity")

    InventoryHolder(inventory.map(pair => if (pair._1 == o) (pair._1, pair._2 - quantity) else pair).filter(_._2 > 0))
  }

  def add(o: Item, quantity: Int): InventoryHolder = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    InventoryHolder(inventory.updated(o, inventory.withDefaultValue(0)(o) + quantity))
  }

  def canTake(o: Item, quantity: Int): Boolean =
    quantity > 0 && inventory.getOrElse(o, 0) < quantity

  /**
    * Transfer objects from the current inventory to an other one. This operation returns a triple, containing the
    * success status of the operation, the updated source inventory, and the updated target inventory.
    *
    * @param target   the target inventory
    * @param o        the object to transfer
    * @param quantity the quantity of objects to transfer
    * @return a triple (success, source, target)
    */
  def transferTo(target: InventoryHolder, o: Item, quantity: Int): (Boolean, InventoryHolder, InventoryHolder) = {
    if (canTake(o, quantity)) {
      (true, take(o, quantity), target.add(o, quantity))
    } else (false, this, target)
  }
}


