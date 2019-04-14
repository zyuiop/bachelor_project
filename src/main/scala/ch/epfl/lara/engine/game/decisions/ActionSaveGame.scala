package ch.epfl.lara.engine.game.decisions

import java.io.PrintStream

import ch.epfl.lara.engine.game.{Game, LevelState}

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionSaveGame() extends Action {
  /**
    * Returns the result of executing this action on a given level state
    *
    * @param inState the state of the level at the beggining
    * @param out     a print stream
    * @return the state of the level after executing this action
    */
  override def execute(inState: LevelState)(implicit out: PrintStream): LevelState = {
    Game.saveGame(inState)
    inState
  }
}

object ActionSaveGame extends ActionBuilder[ActionSaveGame] {
  /**
    * Build the action from the complete user input
    *
    * @param input the user input, split by spaces
    * @return an optional action
    */
  override def apply(input: Array[String]): Try[Action] = Try {
    ActionSaveGame()
  }


  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = Set("save")
}