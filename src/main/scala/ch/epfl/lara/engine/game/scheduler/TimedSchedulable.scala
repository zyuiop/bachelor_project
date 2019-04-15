package ch.epfl.lara.engine.game.scheduler

import ch.epfl.lara.engine.game.PlayerState

/**
  * @author Louis Vialar
  */
trait TimedSchedulable extends Schedulable {
  val nextAction: (Int, (Int, PlayerState) => Option[TimedSchedulable])

  override def runTicks(startTime: Int, endTime: Int)(implicit state: PlayerState): Option[TimedSchedulable] = {
    if (startTime < nextAction._1 && endTime >= nextAction._1) nextAction._2(endTime, state)
    else Some(this)

  }
}
