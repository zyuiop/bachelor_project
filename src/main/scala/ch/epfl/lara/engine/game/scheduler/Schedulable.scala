package ch.epfl.lara.engine.game.scheduler

import ch.epfl.lara.engine.game.LevelState

/**
  * @author Louis Vialar
  */
trait Schedulable {
  def runTicks(startTime: Int, endTime: Int)(implicit state: LevelState): Option[this.type]
}
