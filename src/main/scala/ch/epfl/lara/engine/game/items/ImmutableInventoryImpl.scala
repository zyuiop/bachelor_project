package ch.epfl.lara.engine.game.items

/**
  * Represents an inventory
  *
  * @author Louis Vialar
  */
case class ImmutableInventoryImpl(content: Map[Pickable, Int], name: String) extends Inventory {
  def take(o: Pickable, quantity: Int): ImmutableInventoryImpl = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (content.getOrElse(o, 0) < quantity)
      throw new IllegalStateException("insufficient quantity")

    ImmutableInventoryImpl(content.map(pair => if (pair._1 == o) (pair._1, pair._2 - quantity) else pair).filter(_._2 > 0), name)
  }

  def add(o: Pickable, quantity: Int): ImmutableInventoryImpl = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    ImmutableInventoryImpl(content.updated(o, content.withDefaultValue(0)(o) + quantity), name)
  }

  def canTake(o: Pickable, quantity: Int): Boolean =
    quantity > 0 && content.getOrElse(o, 0) >= quantity

  override def getContent: Map[Pickable, Int] = content
}

object ImmutableInventoryImpl {
  implicit class ImplicitImmutableInventoryImpl(map: Map[Pickable, Int]) extends ImmutableInventoryImpl(map, "Inventory") {

  }
}


