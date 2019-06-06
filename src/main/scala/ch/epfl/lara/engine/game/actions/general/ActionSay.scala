package ch.epfl.lara.engine.game.actions.general

import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder}
import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.messaging.Message.TalkingMessage

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionSay(what: String) extends Action {
  override def apply(inState: CharacterState): Int = {
    inState.talk(what)

    1 + what.split(" ").length / 3
  }
}

object ActionSay extends ActionBuilder {
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionSay(input.drop(1).mkString(" "))
  }

  override val triggeringKeywords: Set[String] = Set("say")
}



