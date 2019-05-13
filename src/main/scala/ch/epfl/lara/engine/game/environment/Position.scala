package ch.epfl.lara.engine.game.environment

/**
  * @author Louis Vialar
  */
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
    case compound@CompoundPosition(_, _) => compound.orientationList == orientationList
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
        case _ => throw new IllegalArgumentException("invalid orientation " + orientation)
      }
    }
  }
}
