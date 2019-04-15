package ch.epfl.lara.engine.game.scheduler

import ch.epfl.lara.engine.game.PlayerState

/**
  * @author Louis Vialar
  */
trait Schedulable {
  def runTicks(startTime: Int, endTime: Int)(implicit state: PlayerState): Option[Schedulable]
}
