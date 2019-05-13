package ch.epfl.lara.engine.game.scheduler

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class Scheduler(startTime: Int) {
  var currentTime: Int = startTime
  val runnables: mutable.PriorityQueue[Schedulable] = mutable.PriorityQueue[Schedulable]()(ord = Ordering.by[Schedulable, Int](s => s.nextRun).reverse)

  def dayTime: Int = Scheduler.timeToDayTime(currentTime)

  def schedule(schedulable: Schedulable): Unit = runnables.enqueue(schedulable)

  def runOnce(inTicks: Int)(runnable: (Int, Int) => Unit): Unit = schedule(new Schedulable {
    override val nextRun: Int = currentTime + inTicks

    override def run(tick: Int): Option[Schedulable] = {
      runnable(tick, nextRun)
      None
    }
  })

  def runRegular(inTicks: Int, everyTick: Int)(runnable: Int => Unit): Unit = {
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
    val (wait, every) = if (diff > 5000) (1, 10) else if (diff > 200) (5000 / diff, 10) else (25, 1) // if (diff > 500) 10 else 50 // * 1000 vs * 100 vs * 20

    var rest = diff
    while (rest > 0) {
      rest -= 1
      currentTime += 1

      val start = System.currentTimeMillis()
      runTick(currentTime)
      val len = System.currentTimeMillis() - start

      if (rest % every == 0 && len < wait)
        Thread.sleep(wait - len)
    }
  }
}

object Scheduler {
  val OneDay = 86400

  def timeToDayTime(time: Int): Int = time % OneDay
}
