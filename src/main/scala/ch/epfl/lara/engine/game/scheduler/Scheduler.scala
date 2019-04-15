package ch.epfl.lara.engine.game.scheduler

import ch.epfl.lara.engine.game.PlayerState

/**
  * @author Louis Vialar
  */
case class Scheduler(currentTime: Int, runnables: List[Schedulable]) extends Schedulable {
  override def runTicks(startTime: Int, endTime: Int)(implicit state: PlayerState): Option[Scheduler] = {
    val runnables = this.runnables.flatMap(r => r.runTicks(startTime, endTime))

    if (runnables.nonEmpty) Some(Scheduler(endTime, runnables))
    else None
  }

  def addTime(diff: Int): Option[Scheduler] = {
    runTicks(currentTime, currentTime + diff)(???)
  }

  def schedule(schedule: Schedulable*): Scheduler = {
    Scheduler(currentTime, runnables ++ schedule)
  }
}
