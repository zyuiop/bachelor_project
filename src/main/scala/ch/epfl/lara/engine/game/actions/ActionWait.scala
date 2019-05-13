package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.entities.CharacterState

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionWait(time: Int) extends Action {
  override def apply(inState: CharacterState): Int = {
    inState.ps.println("You wait " + time + " seconds.")
    time
  }
}

object ActionWait extends ActionBuilder {
  /**
    * Build the action from the complete user input
    *
    * @param input the user input, split by spaces
    * @return an optional action
    */
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionWait(input.drop(1).mkString(" ").toInt) // TODO: improve
  }


  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = Set("wait")
}



