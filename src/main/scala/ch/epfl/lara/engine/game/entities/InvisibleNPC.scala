package ch.epfl.lara.engine.game.entities
import ch.epfl.lara.engine.game.environment.Room
import ch.epfl.lara.engine.game.messaging.Message.SystemMessage

/**
  * @author Louis Vialar
  */
class InvisibleNPC(startState: CharacterState, program: String) extends ProgrammedNPC(startState, program) {
  override def changeRoom(room: Room): Unit = {
    this.currentRoom = room // Don't send any messages
  }

  override def talk(message: String): Unit = {
    currentRoom ! SystemMessage(message)
  }
}
