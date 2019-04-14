package ch.epfl.lara.engine.game.decisions

import java.io.PrintStream

import ch.epfl.lara.engine.game.LevelState
import ch.epfl.lara.engine.game.environment.Position

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionMove(position: Position) extends Action {
  /**
    * Returns the result of executing this action on a given level state
    *
    * @param inState the state of the level at the beggining
    * @param out     a print stream
    * @return the state of the level after executing this action
    */
  override def apply(inState: LevelState, out: PrintStream): LevelState = {
    inState.copy(currentPosition = position)(out)
  }
}

object ActionMove extends ActionBuilder[ActionMove] {
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
  override val triggeringKeywords: Set[String] = Set("move", "go", "walk")
}