package ch.epfl.lara.engine.game.scheduler

/**
  * @author Louis Vialar
  */
trait Schedulable {
  /**
    * the next time this schedulable should run
    */
  val nextRun: Int

  /**
    * Run the schedulable at a given tick. This method is called only if `nextRun` is lower or equal
    * to `tick`.
    * @param tick the current tick
    * @return a new schedulable to schedule
    */
  def run(tick: Int): Option[Schedulable]
}