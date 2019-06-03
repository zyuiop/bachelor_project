package ch.epfl.lara.engine.game.items

/**
  * @author Louis Vialar
  */
trait Item {
  /**
    * The name under which this item can be referenced from the command line
    */
  val displayName: String

  def describe: String = "a " + displayName
}
