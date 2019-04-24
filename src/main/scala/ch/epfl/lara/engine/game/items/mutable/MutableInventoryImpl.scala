package ch.epfl.lara.engine.game.items.mutable

import ch.epfl.lara.engine.game.items.{Inventory, Pickable}

import scala.collection.mutable

/**
  * Represents an inventory
  *
  * @author Louis Vialar
  */
class MutableInventoryImpl(initialContent: Map[Pickable, Int], val name: String) extends Inventory {
  private val content = mutable.Map(initialContent.toList: _*)

  def take(o: Pickable, quantity: Int): MutableInventoryImpl = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (content.getOrElse(o, 0) < quantity)
      throw new IllegalStateException("insufficient quantity")

    content
      .transform((pickable, amount) => if (pickable == o) amount - quantity else amount)
      .retain((_, amount) => amount > 0)

    this
  }

  def add(o: Pickable, quantity: Int): MutableInventoryImpl = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (content.contains(o))
      content
        .transform((pickable, amount) => if (pickable == o) amount + quantity else amount)
    else content put(o, quantity)

    this
  }

  def canTake(o: Pickable, quantity: Int): Boolean =
    quantity > 0 && content.getOrElse(o, 0) >= quantity

  override def getContent: Map[Pickable, Int] = content.toMap
}

