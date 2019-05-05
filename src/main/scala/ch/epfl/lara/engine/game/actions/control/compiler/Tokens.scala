package ch.epfl.lara.engine.game.actions.control.compiler

import scala.util.parsing.input.Positional


object Tokens {

  trait Token extends Positional

  case class And() extends Token

  case class Or() extends Token

  case class In() extends Token

  case class Not() extends Token

  case class Dot() extends Token

  case class Plus() extends Token

  case class Eq() extends Token

  case class Neq() extends Token

  case class Lte() extends Token

  case class Lt() extends Token

  case class Ht() extends Token

  case class Hte() extends Token

  case class Identifier(name: String) extends Token

  case class IntLiteral(value: Int) extends Token

  case class StringLiteral(value: String) extends Token

  case class BooleanLiteral(value: Boolean) extends Token

  case class If() extends Token
  case class Else() extends Token
  case class When() extends Token

  case class LBracket() extends Token
  case class RBracket() extends Token

  case class Do() extends Token
  case class DoNow() extends Token

  case class LPar() extends Token
  case class RPar() extends Token

}
