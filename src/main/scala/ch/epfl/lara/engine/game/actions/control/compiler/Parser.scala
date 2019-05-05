package ch.epfl.lara.engine.game.actions.control.compiler

import ch.epfl.lara.engine.game.actions.control.compiler.Tokens._
import ch.epfl.lara.engine.game.actions.control.compiler.Tree.{Concat, Expr}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
  * @author Louis Vialar
  */
object Parser extends Parsers {
  override type Elem = Tokens.Token

  def identifier: Parser[Tree.Identifier] = {
    def identifierPart: Parser[Tree.Identifier] = accept("identifier", { case Identifier(id) => Tree.Identifier(List(id)) })

    def dotParser: Parser[(Tree.Identifier, Tree.Identifier) => Tree.Identifier] = Dot ^^^ { (l: Tree.Identifier, r: Tree.Identifier) =>
      Tree.Identifier(l.parts ::: r.parts)
    }


    chainl1(identifierPart, dotParser)
  }

  def literals: Parser[Tree.Literal] = accept("literal", {
    case BooleanLiteral(b) => Tree.BooleanLiteral(b)
    case StringLiteral(s) => Tree.StringLiteral(s)
    case IntLiteral(i) => Tree.IntLiteral(i)
  })

  def concat: Parser[Tree.Value] = chainl1(identifier | literals, value, Plus ^^^ {
    (l: Tree.Value, r: Tree.Value) => Concat(l, r)
  })

  def value: Parser[Tree.Value] = concat | identifier | literals

  def comparison: Parser[Tree.Comparison] = {
    def comparisonOperator = Eq | Neq | Lte | Lt | Hte | Ht | In

    value ~ comparisonOperator ~ value ^^ {
      case l ~ Eq ~ r => Tree.Eq(l, r)
      case l ~ Neq ~ r => Tree.Neq(l, r)
      case l ~ Lte ~ r => Tree.Lte(l, r)
      case l ~ Lt ~ r => Tree.Lt(l, r)
      case l ~ Hte ~ r => Tree.Hte(l, r)
      case l ~ Ht ~ r => Tree.Ht(l, r)
      case l ~ In ~ r => Tree.In(l, r)
    }
  }

  def expr: Parser[Expr] = {
    def notExpr = Not ~ expr ^^ { case not ~ e => Tree.Not(e) }

    def orExpr = comparison ~ Or ~ expr ^^ { case l ~ or ~ r => Tree.Or(l, r) }

    def andExpr = comparison ~ And ~ expr ^^ { case l ~ and ~ r => Tree.And(l, r) }

    orExpr | andExpr | notExpr | comparison
  }

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def rest: Reader[Token] = new TokenReader(tokens.tail)

    override def pos: Position = NoPosition

    override def atEnd: Boolean = tokens.isEmpty
  }

  def apply(tokens: Seq[Token]): Either[CompileError, Tree.Expr] = {
    val reader = new TokenReader(tokens)
    expr(reader) match {
      case NoSuccess(msg, next) => Left(CompileError(msg))
      case Success(result, next) => Right(result)
    }
  }
}
