package ch.epfl.lara.engine.game.scheduler

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class Scheduler(startTime: Int) {
  var currentTime: Int = startTime
  val runnables: mutable.PriorityQueue[Schedulable] = mutable.PriorityQueue()(ord = Ordering.by(s => s.nextRun))

  def schedule(schedulable: Schedulable): Unit = runnables.enqueue(schedulable)

  def runOnce(runnable: Int => Unit, inTicks: Int): Unit = schedule(new Schedulable {
    override val nextRun: Int = currentTime + inTicks

    override def run(tick: Int): Option[Schedulable] = {
      runnable(tick)
      None
    }
  })

  def runRegular(runnable: Int => Unit, inTicks: Int, everyTick: Int): Unit = {
    def generate(next: Int): Schedulable = new Schedulable {
      override val nextRun: Int = next

      override def run(tick: Int): Option[Schedulable] = {
        runnable(tick)
        Some(generate(nextRun + everyTick))
      }
    }

    schedule(generate(currentTime + inTicks))
  }

  @tailrec
  private def runTick(tick: Int): Unit = {
    if (runnables.nonEmpty && runnables.head.nextRun <= tick) {
      runnables.dequeue().run(tick).foreach(schedule)
      runTick(tick)
    }
  }

  def addTime(diff: Int): Unit = {
    currentTime += diff
    runTick(currentTime)
  }
}
