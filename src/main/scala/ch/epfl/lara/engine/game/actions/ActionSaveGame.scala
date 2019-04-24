package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.{CharacterState, Game}

import scala.util.Try

/**
  * @author Louis Vialar
  */
case object ActionSaveGame extends Action with ActionBuilder {
  override def apply(inState: CharacterState): Int = {
    Game.saveGame(inState)
    0
  }


  override def apply(input: Array[String]): Try[Action] = Try {
    this
  }

  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = Set("save")
}
