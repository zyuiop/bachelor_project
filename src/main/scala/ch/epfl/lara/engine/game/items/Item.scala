package ch.epfl.lara.engine.game.items

/**
  * This is the base trait for all items, namely things that can be found in rooms.
  * @author Louis Vialar
  */
trait Item {
  /**
    * The name under which this item can be referenced from the command line
    */
  val displayName: String

  /**
    * A description of this item, as shown to the player when he enters the room
    */
  def describe: String = "a " + displayName
}
