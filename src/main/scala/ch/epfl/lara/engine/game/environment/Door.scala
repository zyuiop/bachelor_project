package ch.epfl.lara.engine.game.environment

import java.io.PrintStream

import ch.epfl.lara.engine.game.CharacterState

import scala.util.Random

/**
  * @author Louis Vialar
  */
case class Door(left: String, right: String, leftPos: Position, rightPos: Position, doorType: DoorType, isOpen: CharacterState => Boolean = _ => true) {
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

case class DoorType(name: String, leftToRight: List[String], rightToLeft: List[String] = Nil) {
  def describe(leftToRight: Boolean): String = {
    val list = if (leftToRight || rightToLeft.isEmpty) this.leftToRight else this.rightToLeft
    list(Random.nextInt(list.length))
  }
}

object DoorType {
  val Door = DoorType("door", List("you go through the door"))

  val Stairs = DoorType("staircase", List("you climb the stairs"), List("you descend the stairs"))
}