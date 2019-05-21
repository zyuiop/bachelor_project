package ch.epfl.lara.engine.game.data

import java.io.PrintStream

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.control.ActionCompiler
import ch.epfl.lara.engine.game.control.runner.ConditionExecutionContext
import ch.epfl.lara.engine.game.entities.{CharacterState, PlayerState}
import ch.epfl.lara.engine.game.environment.RoomRegistry
import ch.epfl.lara.engine.game.messaging.Message.SystemMessage

/**
  * @author Louis Vialar
  */
case class LevelDescriptor(rooms: RoomRegistry, entities: List[CharacterState], routines: List[RoutineDescriptor],
                           data: LevelData, playerBuilder: PrintStream => PlayerState) {

  def startLevel(implicit printStream: PrintStream) = {
    // Compile transition
    val success = ActionCompiler.compileValue(data.levelSuccess)
    val failure = ActionCompiler.compileValue(data.levelFailure)

    // Init new GameState
    val startTime = data.startTime

    val state = new GameState(rooms, startTime, data.currency, this, new ConditionExecutionContext(success),
      new ConditionExecutionContext(failure))

    // Init routines
    routines.foreach { case RoutineDescriptor(start, every, message) =>
      val msg = SystemMessage(message)
      state.scheduler.runRegular(if (start >= startTime) start - startTime else 24 * 3600 - start - startTime, every)(_ => {
        state.registry.entities.foreach(_ ! msg)
      })
    }

    // Init characters
    entities.foreach(_.spawn())

    val player = playerBuilder(printStream)

    // Init player
    player.spawn()


    printStream.println(data.startText)

    printStream.println(player.currentRoom.describe())

    // Start scheduler
    state.scheduler.runCurrentTick()

    player
  }
}