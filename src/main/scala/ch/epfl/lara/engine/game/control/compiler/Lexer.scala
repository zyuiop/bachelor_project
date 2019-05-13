package ch.epfl.lara.engine.game.control.compiler

import ch.epfl.lara.engine.game.control.compiler.Tokens._

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

/**
  * @author Louis Vialar
  */
object Lexer extends RegexParsers {
  override val whiteSpace = """(\s|/\*.*\*/)+""".r

  def identifier: Parser[Identifier] = positioned {
    def simpleIdentifier = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) }

    def spacedIdentifier = """`[^`]*`""".r ^^ { str => Identifier(str drop 1 dropRight 1) }

    simpleIdentifier | spacedIdentifier
  }

  def stringLiteral: Parser[StringLiteral] = positioned {
    """"[^"]*"""".r ^^ { str => StringLiteral(str drop 1 dropRight 1) }
  }

  def intLiteral: Parser[IntLiteral] = positioned {
    """[0-9]+""".r ^^ { str => IntLiteral(str.toInt) }
  }

  def timeLiteral: Parser[IntLiteral] = positioned {
    """[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}""".r ^? ( {
      case str =>
        val parts = str.split(":")
        val cnt = parts(0).toInt * 3600 + parts(1).toInt * 60 + parts(2).toInt
        IntLiteral(cnt)
    }, _ => "Invalid time format")
  }

  def and = positioned("&&" ^^^ And())

  def or = positioned("||" ^^^ Or())

  def eq = positioned("==" ^^^ Eq())

  def neq = positioned("!=" ^^^ Neq())

  def lte = positioned("<=" ^^^ Lte())

  def lt = positioned("<" ^^^ Lt())

  def ht = positioned(">" ^^^ Gt())

  def hte = positioned(">=" ^^^ Gte())

  def dot = positioned("." ^^^ Dot())

  def plus = positioned("+" ^^^ Plus())

  def mod = positioned("%" ^^^ Mod())

  def div = positioned("/" ^^^ Div())

  def minus = positioned("-" ^^^ Minus())

  def times = positioned("*" ^^^ Times())

  def in = positioned("in " ^^^ In())

  def not = positioned("!" ^^^ Not())

  def bTrue = positioned("true" ^^^ BooleanLiteral(true))

  def bFalse = positioned("false" ^^^ BooleanLiteral(false))

  def ifs = positioned("if" ^^^ If())

  def elses = positioned("else" ^^^ Else())

  def when = positioned("when" ^^^ When())

  def lbrack = positioned("{" ^^^ LBracket())

  def rbrack = positioned("}" ^^^ RBracket())

  def dos = positioned("do " ^^^ Do())

  def doNow = positioned("now " ^^^ DoNow())

  def lpar = positioned("(" ^^^ LPar())

  def rpar = positioned(")" ^^^ RPar())

  def nulls = positioned("null" ^^^ Null())

  def set = positioned(":=" ^^^ Set())


  def reserved = in | bTrue | bFalse | ifs | elses | when | dos | doNow | nulls

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(positioned((identifier ||| reserved) | lbrack | rbrack | lpar | rpar | stringLiteral | timeLiteral | intLiteral | and | or | eq | neq | lte | lt | hte | ht | dot | plus | minus | div | mod | times | not | set))
    )
  }

  def apply(code: String): Either[CompileError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(CompileError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}
