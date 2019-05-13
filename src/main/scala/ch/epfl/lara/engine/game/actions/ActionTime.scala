package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.entities.CharacterState

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionTime() extends Action {
  override def apply(inState: CharacterState): Int = {
    import ch.epfl.lara.engine.game.scheduler.TimeUtils._

    val time = GameState.scheduler.currentTime

    inState.ps.println("It's currently " + time.timeString)
    0
  }
}

object ActionTime extends ActionBuilder {
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionTime()
  }

  override val triggeringKeywords: Set[String] = Set("time")
}



