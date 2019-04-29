package ch.epfl.lara.engine.game.messaging

import ch.epfl.lara.engine.game.CharacterState

/**
  * @author Louis Vialar
  */
sealed trait Message {
  val sentBy: CharacterState
}

object Message {
  case class TalkingMessage(sentBy: CharacterState, content: String) extends Message

  case class RoomMovement(sentBy: CharacterState, entering: Boolean) extends Message
}
