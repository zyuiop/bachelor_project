package ch.epfl.lara.engine.game.characters

import ch.epfl.lara.engine.game.messaging.Message
import ch.epfl.lara.engine.game.messaging.Message.{ReleasedControl, TakenControl}

/**
  * @author Louis Vialar
  */
class PPC(startState: CharacterState, program: String) extends ProgrammedNPC(startState, program) {
  var controlled = false

  private def takeControl(): Unit = {
    controlled = true
    despawn()
  }

  private def releaseControl(): Unit = {
    controlled = false
    spawn()
  }

  override def handle(message: Message): Unit = {
    message match {
      case TakenControl(_) =>
        super.handle(message)
        ec.runNow
        takeControl()
      case ReleasedControl(_) =>
        releaseControl()
        super.handle(message)
      case _ =>
        super.handle(message)
    }
  }
}

