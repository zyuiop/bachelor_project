package ch.epfl.lara.engine.game.environment

import ch.epfl.lara.engine.game.{SceneState, InventoryHolder}
import ch.epfl.lara.engine.game.decisions.Command

/**
  * @author Louis Vialar
  */
case class Room(doors: Map[Position, Door],
                objects: Map[Position, InventoryHolder]) {

  def describe(): String = ???

  def takeDoor(position: Position): Option[Door] =
    doors.get(position)

  private def newInventory(decision: Command, inventory: List[(Object, Int)]) = ???
}
