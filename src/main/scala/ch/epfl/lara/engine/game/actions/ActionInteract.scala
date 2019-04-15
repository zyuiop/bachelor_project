package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.PlayerState
import ch.epfl.lara.engine.game.environment.Position

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionInteract(elem: String) extends Action {
  /**
    * Returns the result of executing this action on a given level state
    *
    * @param inState the state of the level at the beggining
    * @param out     a print stream
    * @return the state of the level after executing this action
    */
  /*override def apply(inState: PlayerState, out: PrintStream): PlayerState = {
    inState.copy(currentPosition = position)(out)
  }*/
}


object ActionInteract extends ActionBuilder[ActionInteract] {
  /**
    * Build the action from the complete user input
    *
    * @param input the user input, split by spaces
    * @return an optional action
    */
  override def apply(input: Array[String]): Try[Action] = Try {
    val args = input.tail

    if (args.isEmpty) throw new IllegalArgumentException("you need to provide a direction to walk to...")
    else ActionMove(Position.parse(args.mkString("-")))
  }


  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = Set("interact", "talk", "open", "use")
}