package ch.epfl.lara.engine.game.data

import java.io.Reader

import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.environment._

/**
  * @author Louis Vialar
  */
object LevelParser extends BaseParser {
  def room = "[room]" ~ properties ^^ {
    case _ ~ props =>
      val startInv = inventory("inv", props)

      // TODO: interactables

      new Room(props("id"), props("name"), props("ambient"), startInv)

  }

  def doorType = "[doortype]" ~ properties ^^ {
    case _ ~ props =>
      val n = props("name")
      val leftToRight = multiVal("leftToRight", props)
      val rightToLeft = multiVal("rightToLeft", props)

      new DoorType(n, leftToRight, rightToLeft)
  }

  def door = "[door]" ~ properties ^^ {
    case _ ~ props =>
      // Keys
      val keys = multiVal("key", props)
      val attrs = prefixed("attr", props)

      val opening = if (keys.isEmpty && attrs.isEmpty) (_: CharacterState) => true else (state: CharacterState) => {
        // TODO: room for improvement (ability to have OR conditions)
        keys.forall(keyItem => state.inventory.getItemByName(keyItem).isSuccess) &&
          attrs.forall(pair => state.attributes.contains(pair._1) && state.attributes(pair._1) == pair._2)
      }

      (doorTypeGetter: String => DoorType) =>
        Door(props("left"), props("right"), Position.parse(props("leftPos")), Position.parse(props("rightPos")), doorTypeGetter(props("doorType")), opening)
  }

  def file = rep(room | door | doorType) ^^ {
    l => {
      val (types, r1) = l.partition(_.isInstanceOf[DoorType])
      val (rooms, doors) = r1.partition(_.isInstanceOf[Room])

      val doorTypes = types.map { case t: DoorType => t.name -> t } toMap

      RoomRegistry(
        rooms.map(_.asInstanceOf[Room]),
        doors map { case f: ((String => DoorType) => Door) => f(doorTypes) },
        true) // TODO: Remove when possible
    }
  }

  def apply(content: String): RoomRegistry = {
    parse(file, content).get
  }

  def apply(content: Reader): RoomRegistry = {
    parse(file, content).get
  }
}
