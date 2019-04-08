package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.decisions.Command

import scala.util.Try

/**
  * @author Louis Vialar
  */
package object environment {


  case class Door(left: String, right: String, leftPos: Position, rightPos: Position, doorType: DoorType, isOpen: LevelState => Boolean = _ => true) {
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

  sealed trait Position {
    val name: String = this.getClass.getCanonicalName

  }

  case object North extends Position
  case object South extends Position
  case object East extends Position
  case object West extends Position
  case object Center extends Position

  case class CompoundPosition(first: Position, second: Position) extends Position {
    override val name: String = first.name + "-" + second.name

    private def orientationList(orientation: Position): List[Position] = orientation match {
      case CompoundPosition(first, second) => orientationList(first) ++ orientationList(second)
      case _ => List(orientation)
    }

    private val orientationList: List[Position] = orientationList(this)

    override def equals(obj: Any): Boolean = obj match {
      case compound @ CompoundPosition(_, _) => compound.orientationList == orientationList
      case _ => super.equals(obj)
    }

    override def hashCode(): Int = orientationList.hashCode()
  }

  object Position {
    def parse(orientation: String): Position = {
      if (orientation.contains("-")) {
        val (head, tail) = orientation.splitAt(orientation.indexOf("-"))
        CompoundPosition(parse(head), parse(tail.tail))
      } else {
        orientation.toLowerCase() match {
          case "north" => North
          case "south" => South
          case "east" => East
          case "west" => West
          case "center" => Center
          case _ => throw new IllegalArgumentException("invalid orientation")
        }
      }
    }

    def parseEither(orientation: String): Either[Throwable, Position] = {
      Try(parse(orientation)).toEither
    }
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
}
