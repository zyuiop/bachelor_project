package ch.epfl.lara.engine.game.items

/**
  * @author Louis Vialar
  */
trait Item {
  /**
    * Check if this item can be picked up and stored in player's inventory
    * @return
    */
  def canPick: Boolean

  /**
    * Check if this item can be interacted with by the player
    * @return
    */
  def canInteract: Boolean
}
