package ch.epfl.lara.engine.game.items

import scala.collection.mutable

/**
  * Represents an inventory
  *
  * @author Louis Vialar
  */
class Inventory(initialContent: Map[Storable, Int], val name: String) extends InventoryLike {
  private val content = mutable.Map(initialContent.toList: _*)

  def take(o: Storable, quantity: Int): Inventory = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (content.getOrElse(o, 0) < quantity)
      throw new IllegalStateException("insufficient quantity")

    content
      .transform((pickable, amount) => if (pickable == o) amount - quantity else amount)
      .retain((_, amount) => amount > 0)

    this
  }

  def add(o: Storable, quantity: Int): Inventory = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (content.contains(o))
      content
        .transform((pickable, amount) => if (pickable == o) amount + quantity else amount)
    else content put(o, quantity)

    this
  }

  def canTake(o: Storable, quantity: Int): Boolean =
    quantity > 0 && content.getOrElse(o, 0) >= quantity

  override def getContent: Map[Storable, Int] = content.toMap
}

