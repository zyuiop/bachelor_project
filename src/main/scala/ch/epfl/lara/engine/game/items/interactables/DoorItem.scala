package ch.epfl.lara.engine.game.items.interactables

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.items.{Interactable, Item}
import ch.epfl.lara.engine.game.messaging.Message.RoomMovement

/**
  * @author Louis Vialar
  */
class DoorItem(val displayName: String, targetRoom: String, description: String) extends Item with Interactable {
  override def interact(state: CharacterState): Int = {
    state.ps.println(description.capitalize + ".")

    val room = GameState.level.getRoom(targetRoom)
    val prev = state.currentRoom

    state.ps.println(room.describe())

    room ! RoomMovement(state, entering = true)

    state.currentRoom = room

    prev ! RoomMovement(state, entering = false)

    7
  }

  override def isDoor: Boolean = true

  override def describe: String = super.describe + " leading to " + GameState.level.getRoom(targetRoom).name
}
