package ch.epfl.lara.engine.game.messaging

/**
  * @author Louis Vialar
  */
sealed trait Message {}

object Message {
  case class TalkingMessage(sentBy: String, content: String) extends Message

  case class RoomMovement(sentBy: String, entering: Boolean) extends Message
}
