package ch.epfl.lara.engine.game.actions.control.runner

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.actions.control.compiler.Tree
import ch.epfl.lara.engine.game.scheduler.Scheduler

/**
  * @author Louis Vialar
  */
class GlobalConditionExecutionContext(program: Tree.Value) extends BaseExecutionContext {
  private def currentTime = GameState.scheduler.currentTime

  private def env(additionnal: Map[String, Environment] = Map()): Environment = MapEnvironment(
      // Actual env, overrides self attributes if same name
      Map(
        "time" -> ValueEnvironment(Scheduler.timeToDayTime(currentTime).toString),
        "totalTime" -> ValueEnvironment(currentTime.toString),
        "characters" -> PassByNameEnvironment(() => MapEnvironment(
          GameState.registry.entities.map(state => (state.name, ObjectMappingEnvironment(state))).toMap +
            ("player" -> ObjectMappingEnvironment(GameState.registry.player))
        )),
        "rooms" -> PassByNameEnvironment(() =>
          MapEnvironment(GameState.level.rooms.mapValues(l => ObjectMappingEnvironment(l)))),
        "state" -> PassByNameEnvironment(() => ObjectMappingEnvironment(GameState))
      ) ++ additionnal)

  def checkCondition(): Boolean = {
    implicit val env: Environment = this.env()
    valueAsBoolean(resolve(program))
  }
}
