package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.environment.{Center, Room}

/**
  * @author Louis Vialar
  */
class PlayerState(startRoom: Room) extends CharacterState(startRoom, Center, "you") {

}
