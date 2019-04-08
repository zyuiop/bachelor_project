package ch.epfl.lara.engine.game.scheduler

import ch.epfl.lara.engine.game.{GameState, LevelState}

/**
  * @author Louis Vialar
  */
case class OneRunSchedulable(runAtTime: Int, runner: LevelState => Unit) extends TimedSchedulable {
  override val nextAction: (Int, (Int, LevelState) => Option[TimedSchedulable]) =
    (runAtTime, (_: Int, state: LevelState) => { runner(state) ; Option.empty[TimedSchedulable] })
}
