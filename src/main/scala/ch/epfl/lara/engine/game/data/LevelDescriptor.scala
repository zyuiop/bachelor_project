package ch.epfl.lara.engine.game.data

import java.io.PrintStream

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.control.ActionCompiler
import ch.epfl.lara.engine.game.control.runner.ConditionExecutionContext
import ch.epfl.lara.engine.game.characters.{CharacterState, PlayerState}
import ch.epfl.lara.engine.game.environment.Room
import ch.epfl.lara.engine.game.items.Storable
import ch.epfl.lara.engine.game.messaging.Message.SystemMessage

/**
  * @author Louis Vialar
  */
case class LevelDescriptor(rooms: Map[String, Room], characters: List[CharacterState], routines: List[RoutineDescriptor],
                           data: LevelData, playerBuilder: (PrintStream, Option[String] => Unit) => PlayerState) {

  def startLevel(implicit printStream: PrintStream, imageSetter: Option[String] => Unit) = {
    // Compile transition
    val success = ActionCompiler.compileValue("level success", data.levelSuccess)
    val failure = ActionCompiler.compileValue("level failure", data.levelFailure)

    // Init new GameState
    val startTime = data.startTime

    val state = new GameState(rooms, startTime, data.currency, this, new ConditionExecutionContext(success),
      new ConditionExecutionContext(failure))

    // Init routines
    routines.foreach { case RoutineDescriptor(start, every, message) =>
      val msg = SystemMessage(message)
      state.scheduler.runRegular(if (start >= startTime) start - startTime else 24 * 3600 - start - startTime, every)(_ => {
        state.registry.characters.foreach(_ ! msg)
      })
    }

    // Init characters
    characters.foreach(_.spawn())

    val player = playerBuilder(printStream, imageSetter)

    // Init player
    player.spawn()


    printStream.println(data.startText)

    printStream.println(player.currentRoom.describe())

    // Start scheduler
    state.scheduler.runCurrentTick()

    player
  }
}

case class LevelData(currency: Storable, name: String, startText: String, endText: String,
                     levelSuccess: String, levelFailure: String, startTime: Int)

case class RoutineDescriptor(startTime: Int, runEvery: Int, message: String)
