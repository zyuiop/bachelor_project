package ch.epfl.lara.engine.game.environment

import java.io.PrintStream

import ch.epfl.lara.engine.game.PlayerState

/**
  * @author Louis Vialar
  */
case class Door(left: String, right: String, leftPos: Position, rightPos: Position, doorType: DoorType, isOpen: PlayerState => Boolean = _ => true) {
  def use(from: Room)(implicit out: PrintStream): (String, Position) = {
    val leftToRight = from.id == left
    out.println(doorType.describe(leftToRight).capitalize + ".")

    (if (leftToRight) right else left, if (leftToRight) rightPos else leftPos)
  }

  def getTargetRoom(from: Room): String = {
    if (from.id == left) right else left
  }

  def getPosition(room: Room): Position = if (room.id == left) leftPos else rightPos
}

trait DoorType {
  val name: String

  def describe(leftToRight: Boolean): String
}

object DoorType {

  case object Door extends DoorType {
    override def describe(leftToRight: Boolean): String = "you go though the door"

    override val name = "door"
  }

  case object Stairs extends DoorType {
    override val name = "staircase"

    override def describe(leftToRight: Boolean): String =
      if (leftToRight) "you climb the stairs"
      else "you descend the stairs"

    // TODO: pick a word randomly!
  }

}