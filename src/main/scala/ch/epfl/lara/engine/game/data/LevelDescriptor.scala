package ch.epfl.lara.engine.game.data

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.actions.control.ActionCompiler
import ch.epfl.lara.engine.game.actions.control.runner.GlobalConditionExecutionContext
import ch.epfl.lara.engine.game.entities.{CharacterState, PlayerState}
import ch.epfl.lara.engine.game.environment.RoomRegistry
import ch.epfl.lara.engine.game.messaging.Message.SystemMessage

/**
  * @author Louis Vialar
  */
case class LevelDescriptor(rooms: RoomRegistry, entities: List[CharacterState], routines: List[RoutineDescriptor],
                           data: LevelData, player: PlayerState) {

  def startLevel() = {
    // Compile transition
    val success = ActionCompiler.compileValue(data.levelSuccess)
    val failure = ActionCompiler.compileValue(data.levelFailure)

    // Init new GameState
    val startTime = data.startTime

    val state = new GameState(rooms, startTime, data.currency, this, new GlobalConditionExecutionContext(success),
      new GlobalConditionExecutionContext(failure))

    // Init routines
    routines.foreach { case RoutineDescriptor(start, every, message) =>
      val msg = SystemMessage(message)
      state.scheduler.runRegular(if (start >= startTime) start - startTime else 24 * 3600 - start - startTime, every)(_ => {
        state.registry.entities.foreach(_ ! msg)
      })
    }

    // Init characters
    entities.foreach(_.spawn())

    // Init player
    player.spawn()


    println(data.startText)

    println(player.currentRoom.describe())

    player
  }
}