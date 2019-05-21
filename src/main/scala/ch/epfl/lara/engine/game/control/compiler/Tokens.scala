package ch.epfl.lara.engine.game.control.compiler

import scala.util.parsing.input.Positional


object Tokens {

  class Token(name: String) extends Positional {
    override def toString: String = name
  }

  case class And() extends Token("&&")

  case class Or() extends Token("||")

  case class In() extends Token("in")

  case class Not() extends Token("!")

  case class Dot() extends Token(".")

  case class Plus() extends Token("+")
  case class Mod() extends Token("%")
  case class Div() extends Token("/")
  case class Times() extends Token("*")
  case class Minus() extends Token("-")

  case class Eq() extends Token("==")

  case class Neq() extends Token("!=")

  case class Lte() extends Token("<=")

  case class Lt() extends Token("<")

  case class Gt() extends Token(">")

  case class Gte() extends Token(">=")

  case class Identifier(name: String) extends Token("Identifier")

  case class IntLiteral(value: Int) extends Token("Int Literal")

  case class StringLiteral(value: String) extends Token("String Literal")

  case class BooleanLiteral(value: Boolean) extends Token("Boolean Literal")

  case class If() extends Token("if")
  case class While() extends Token("while")
  case class Else() extends Token("else")
  case class When() extends Token("when")
  case class On() extends Token("on")

  case class LBracket() extends Token("{")
  case class RBracket() extends Token("}")

  case class Do() extends Token("do")
  case class DoNow() extends Token("now")
  case class DoBlocking() extends Token("blocking")


  case class LPar() extends Token("(")
  case class RPar() extends Token(")")

  case class Null() extends Token("null")
  case class Set() extends Token(":=")

}
