package ch.epfl.lara.engine.game.actions.general

import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder}
import ch.epfl.lara.engine.game.characters.{CharacterState, PlayerState}

import scala.util.Try

/**
  * @author Louis Vialar
  */
case object ActionControlStop extends Action with ActionBuilder {
  override def apply(inState: CharacterState): Int = {
    // Try to leave interact
    if (!inState.isInstanceOf[PlayerState]) {
      return 0
    }

    val player = inState.asInstanceOf[PlayerState]

    if (player.controlled.isEmpty) {
      inState.ps.println("You don't control anyone!")
      0
    } else {
      inState.ps.println(s"You finally release your powers and leave the body of ${player.controlled.get.name}...")
      player.release()
      5
    }
  }

  override val triggeringKeywords: Set[String] = Set("leave", "release")

  override def apply(input: Array[String]): Try[Action] = Try {
    this
  }
}
