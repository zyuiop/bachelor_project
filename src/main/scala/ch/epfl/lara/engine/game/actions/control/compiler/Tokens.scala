package ch.epfl.lara.engine.game.actions.control.compiler


object Tokens {

  trait Token

  case object And extends Token

  case object Or extends Token

  case object In extends Token

  case object Not extends Token

  case object Dot extends Token

  case object Plus extends Token

  case object Eq extends Token

  case object Neq extends Token

  case object Lte extends Token

  case object Lt extends Token

  case object Ht extends Token

  case object Hte extends Token

  case class Identifier(name: String) extends Token

  case class IntLiteral(value: Int) extends Token

  case class StringLiteral(value: String) extends Token

  case class BooleanLiteral(value: Boolean) extends Token

}
