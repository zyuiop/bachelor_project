package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.characters.CharacterState

import scala.util.{Success, Try}

/**
  * A trait that enables adding new commands directly inside of some parts of the game. All the commands defined in
  * implementations of this trait have priority over standard commands if the keyword is shared.
  * @author Louis Vialar
  */
trait ActionInterceptor {
  private var _parser = ActionParser()

  def updateParser(previousParser: ActionParser): ActionParser = {
    _parser union previousParser
  }

  /**
    * Handle a custom command
    * @param words the alternative keywords that trigger the command (must not contain a space, as only the first word is compared)
    * @param action the action to execute when the command is executed. It takes as arguments the character running the command and
    *               the arguments of the command (the input words, splitted on space, without the first word (keyword)), and returns the
    *               game time it took to execute the command, in second.
    */
  final def handle(words: String*)(action: (CharacterState, Array[String]) => Int) = {
    _parser = _parser.addBuilders(new ActionBuilder {
      override def apply(input: Array[String]): Try[Action] = Success((v1: CharacterState) => action(v1, input drop 1))

      override val triggeringKeywords: Set[String] = Set(words :_*)
    })
  }

  /**
    * Try handling a custom command
    * @param words the alternative keywords that can trigger the command (must not contain a space, as only the first word is compared)
    * @param action the action to execute when the command is executed. It takes the arguments of the command (all except the keyword) and
    *               produces a command (Success) or nothing (Failure). The command, when executed, takes as an argument the character
    *               running it and returns the time it took.
    */
  final def tryHandle(words: String*)(action: Array[String] => Try[CharacterState => Int]) = {
    _parser = _parser.addBuilders(new ActionBuilder {
      override def apply(input: Array[String]): Try[Action] = action(input drop 1).map(s => (inState: CharacterState) => s(inState))

      override val triggeringKeywords: Set[String] = Set(words :_*)
    })
  }

  final def parser: ActionParser = _parser
}
