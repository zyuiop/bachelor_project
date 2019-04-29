package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.messaging.Message.TalkingMessage

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionSay(what: String) extends Action {
  override def apply(inState: CharacterState): Int = {
    inState.currentRoom ! TalkingMessage(inState, what)

    1 + what.split(" ").length / 3
  }
}

object ActionSay extends ActionBuilder {
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionSay(input.drop(1).mkString(" "))
  }

  override val triggeringKeywords: Set[String] = Set("say")
}



