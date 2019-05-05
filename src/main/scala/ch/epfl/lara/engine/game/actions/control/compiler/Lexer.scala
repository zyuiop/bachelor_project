package ch.epfl.lara.engine.game.actions.control.compiler

import ch.epfl.lara.engine.game.actions.control.compiler.Tokens._

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

/**
  * @author Louis Vialar
  */
object Lexer extends RegexParsers {
  def identifier: Parser[Identifier] = {
    def simpleIdentifier = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) }
    def spacedIdentifier = """`[^`]*`""".r ^^ { str => Identifier(str drop 1 dropRight 1) }

    simpleIdentifier | spacedIdentifier
  }

  def stringLiteral: Parser[StringLiteral] = {
    """"[^"]*"""".r ^^ { str => StringLiteral(str drop 1 dropRight 1) }
  }

  def intLiteral: Parser[IntLiteral] = {
    """[0-9]+""".r ^^ { str => IntLiteral(str.toInt) }
  }

  def timeLiteral: Parser[IntLiteral] = {
    """[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}""".r ^? ( {
      case str =>
        val parts = str.split(":")
        val cnt = parts(0).toInt * 3600 + parts(1).toInt * 60 + parts(2).toInt
        IntLiteral(cnt)
    }, _ => "Invalid time format")
  }

  def and = "&&" ^^^ And

  def or = "||" ^^^ Or

  def eq = "==" ^^^ Eq

  def neq = "!=" ^^^ Neq

  def lte = "<=" ^^^ Lte

  def lt = "<" ^^^ Lt

  def ht = ">" ^^^ Ht

  def hte = ">=" ^^^ Hte

  def dot = "." ^^^ Dot

  def plus = "+" ^^^ Plus

  def in = "in " ^^^ In
  def not = "!" ^^^ Not
  def bTrue = "true" ^^^ BooleanLiteral(true)
  def bFalse = "false" ^^^ BooleanLiteral(false)

  def ifs = "if " ^^^ If
  def elses = "else " ^^^ Else
  def when = "when " ^^^ When
  def lbrack = "{" ^^^ LBracket
  def rbrack = "}" ^^^ RBracket
  def dos = "do " ^^^ Do
  def doNow = "now " ^^^ DoNow
  def lpar = "(" ^^^ LPar
  def rpar = ")" ^^^ RPar

  def reserved = in | bTrue | bFalse | ifs | elses | when | dos | doNow

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(reserved | not | lbrack | rbrack | lpar | rpar | identifier | stringLiteral | timeLiteral | intLiteral | and | or | eq | neq | lte | lt | hte | ht | dot | plus)
    )
  }

  def apply(code: String): Either[CompileError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => Left(CompileError(msg))
      case Success(result, _) => Right(result)
    }
  }
}
