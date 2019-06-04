package ch.epfl.lara.engine.game.items

/**
  * An item that can be stored in an inventory
  * @author Louis Vialar
  */
trait Storable extends Item {
  // Register the item
  Storable.registry.put(displayName, this)
}

object Storable {
  // The regstry of all storable items
  private val registry: collection.mutable.Map[String, Storable] = collection.mutable.Map()

  case class SimpleStorable(displayName: String) extends Storable {
    override def toString: String = displayName
  }

  def apply(name: String): Storable = {
    registry.getOrElse(name, SimpleStorable(name))
  }
}