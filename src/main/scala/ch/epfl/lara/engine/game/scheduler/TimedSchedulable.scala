package ch.epfl.lara.engine.game.scheduler

import ch.epfl.lara.engine.game.LevelState

/**
  * @author Louis Vialar
  */
trait TimedSchedulable extends Schedulable {
  val nextAction: (Int, (Int, LevelState) => Option[TimedSchedulable])

  override def runTicks(startTime: Int, endTime: Int)(implicit state: LevelState): Option[TimedSchedulable] = {
    if (startTime < nextAction._1 && endTime >= nextAction._1) nextAction._2(endTime, state)
    else Some(this)

  }
}
