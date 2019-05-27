package ch.epfl.lara.engine.game.items

/**
  * @author Louis Vialar
  */
trait Pickable extends Item {
  // Register the item
  Pickable.registry.put(displayName, this)
}

object Pickable {
  private val registry: collection.mutable.Map[String, Pickable] = collection.mutable.Map()

  case class SimplePickable(displayName: String) extends Pickable {
    override def toString: String = displayName
  }

  def apply(name: String): Pickable = {
    registry.getOrElse(name, SimplePickable(name))
  }
}