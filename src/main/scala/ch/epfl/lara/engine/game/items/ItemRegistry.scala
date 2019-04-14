package ch.epfl.lara.engine.game.items

/**
  * @author Louis Vialar
  */
case class ItemRegistry(registry: Map[String, () => Item]) {
  def addItem(key: String, builder: () => Item) = ItemRegistry(registry + (key.toLowerCase -> builder))

  def getItem(key: String, keyMightBePlural: Boolean = false): Option[() => Item] = {
    val opt = registry.get(key.toLowerCase)

    if (keyMightBePlural && key.endsWith("s")) opt.orElse(getItem(key.dropRight(1)))
    else opt
  }
}