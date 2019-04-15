package ch.epfl.lara.engine.game.scheduler

import ch.epfl.lara.engine.game.{GameState, PlayerState}

/**
  * @author Louis Vialar
  */
case class OneRunSchedulable(runAtTime: Int, runner: PlayerState => Unit) extends TimedSchedulable {
  override val nextAction: (Int, (Int, PlayerState) => Option[TimedSchedulable]) =
    (runAtTime, (_: Int, state: PlayerState) => { runner(state) ; Option.empty[TimedSchedulable] })
}
