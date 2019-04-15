package ch.epfl.lara.engine.game.scheduler

/**
  * @author Louis Vialar
  */
object TimeUtils {
  implicit class TimeWrapper(timestamp: Int) {
    val seconds = timestamp % 60

    val minutes = (timestamp / 60) % 60

    val hours = (timestamp / 3600) % 24

    val days = timestamp / (3600 * 24)

    val timeString = {
      def prependZero(time: Int) = {
        if (time < 10) "0" + time
        else "" + time
      }

      // TODO: improve.
      hours + ":" + prependZero(minutes) + ":" + prependZero(seconds)
    }
  }
}
