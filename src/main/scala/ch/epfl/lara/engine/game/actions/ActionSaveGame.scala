package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.{Game, CharacterState}

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionSaveGame() extends Action {
  override def apply(inState: CharacterState): Int = {
    Game.saveGame(inState)
    0
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