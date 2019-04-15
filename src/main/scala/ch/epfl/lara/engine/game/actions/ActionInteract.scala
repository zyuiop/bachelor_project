package ch.epfl.lara.engine.game.actions

import java.io.PrintStream

import ch.epfl.lara.engine.game.PlayerState

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionInteract(objectName: String) extends Action {
  /**
    * Returns the result of executing this action on a given level state
    *
    * @param inState the state of the level at the beggining
    * @param out     a print stream
    * @return the state of the level after executing this action
    */
  override def apply(inState: PlayerState, out: PrintStream): (PlayerState, Int) = {
    inState.currentRoom
      .getInteractableItem(objectName, inState.currentPosition)
      .map(_.interact(inState)(out))
      .map((_, 5))
      .getOrElse {
      println("there is nothing to interact here...")
        (inState, 0)
    }
  }
}

object ActionInteract extends ActionBuilder[ActionInteract] {
  /**
    * Build the action from the complete user input
    *
    * @param input the user input, split by spaces
    * @return an optional action
    */
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionInteract(input.drop(1).mkString(" ").toLowerCase)
  }


  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = Set("interact", "use", "open")
}
