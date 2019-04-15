package ch.epfl.lara.engine.game.items

/**
  * @author Louis Vialar
  */
trait Pickable extends Item {

}

object Pickable {
  case class SimplePickable(displayName: String) extends Pickable

  def apply(name: String): Pickable = SimplePickable(name)
}