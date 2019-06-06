package ch.epfl.lara.engine.game.items.interactables

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.characters.CharacterState
import ch.epfl.lara.engine.game.items.{Interactable, Item}
import ch.epfl.lara.engine.game.messaging.Message.RoomMovement

import scala.util.Random

/**
  * @author Louis Vialar
  */
class DoorItem(val displayName: String, targetRoom: String, description: List[String]) extends Item with Interactable {
  override def interact(state: CharacterState): Int = {
    state.ps.println(description(Random.nextInt(description.size)).capitalize + ".")
    val room = GameState.level(targetRoom)
    state.changeRoom(room)
    7
  }

  override def describe: String = super.describe + " leading to " + GameState.level(targetRoom).name
}
