package ch.epfl.lara.engine.game.entities

import java.io.PrintStream

import ch.epfl.lara.engine.game.control.ActionCompiler
import ch.epfl.lara.engine.game.control.runner.CharacterExecutionContext
import ch.epfl.lara.engine.game.messaging.Message

/**
  * @author Louis Vialar
  */

class ProgrammedNPC(startState: CharacterState,
                    program: String) extends CharacterState(
  startState.currentRoom, startState.currentPosition, startState.name,
  startState.inventory.getContent, startState.attributes, new PrintStream(_ => ())
) with NPC {
  private val (prog, triggers) = ActionCompiler.compileProgram(program)

  protected val ec = new CharacterExecutionContext(prog, triggers, this)

  override def spawn(): Unit = {
    super.spawn()
    ec.start()
  }

  override def despawn(): Unit = {
    super.despawn()
    ec.stop()
  }

  override def handle(message: Message): Unit = {
    super.handle(message)

    ec ! message // let the triggers handle it
  }
}
