package ch.epfl.lara.engine.game.messaging

/**
  * @author Louis Vialar
  */
trait MessageHandler {
  def handle(message: Message): Unit

  def !(message: Message): Unit = handle(message)
}
