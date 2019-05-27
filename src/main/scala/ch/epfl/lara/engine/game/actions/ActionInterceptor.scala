package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.entities.CharacterState

import scala.util.{Success, Try}

/**
  * @author Louis Vialar
  */
trait ActionInterceptor {
  private var _parser = ActionParser()

  def updateParser(previousParser: ActionParser): ActionParser = {
    _parser union previousParser
  }

  final def handle(words: String*)(action: (CharacterState, Array[String]) => Int) = {
    _parser = _parser.addBuilders(new ActionBuilder {
      override def apply(input: Array[String]): Try[Action] = Success(new Action {
        override def apply(v1: CharacterState): Int = action(v1, input drop 1)
      })

      override val triggeringKeywords: Set[String] = Set(words :_*)
    })
  }

  final def parser: ActionParser = _parser
}
