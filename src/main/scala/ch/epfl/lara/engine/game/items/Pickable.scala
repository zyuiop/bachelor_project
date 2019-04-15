package ch.epfl.lara.engine.game.items

/**
  * @author Louis Vialar
  */
trait Pickable extends Item {

}

object Pickable {
  def apply(name: String): Pickable = new Pickable {
    /**
      * The name under which this item can be referenced from the command line
      */
    override val displayName: String = name
  }
}