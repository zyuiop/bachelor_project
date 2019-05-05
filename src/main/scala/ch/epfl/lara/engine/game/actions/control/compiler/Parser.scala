package ch.epfl.lara.engine.game.actions.control.compiler

import ch.epfl.lara.engine.game.actions.control.compiler.Tokens._
import ch.epfl.lara.engine.game.actions.control.compiler.Tree.{Concat, EmptyExpr, Ite, LogicalExpression, Sequence}

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

    def comp = value ~ comparisonOperator ~ value ^^ {
      case l ~ Eq ~ r => Tree.Eq(l, r)
      case l ~ Neq ~ r => Tree.Neq(l, r)
      case l ~ Lte ~ r => Tree.Lte(l, r)
      case l ~ Lt ~ r => Tree.Lt(l, r)
      case l ~ Hte ~ r => Tree.Hte(l, r)
      case l ~ Ht ~ r => Tree.Ht(l, r)
      case l ~ In ~ r => Tree.In(l, r)
    }

    def boolLit = accept("boolean", { case BooleanLiteral(b) => Tree.BooleanLiteral(b) })

    comp | boolLit
  }

  def logicalExpr: Parser[LogicalExpression] = {
    def notExpr = Not ~ logicalExpr ^^ { case not ~ e => Tree.Not(e) }

    def orExpr = basic ~ Or ~ logicalExpr ^^ { case l ~ or ~ r => Tree.Or(l, r) }

    def andExpr = basic ~ And ~ logicalExpr ^^ { case l ~ and ~ r => Tree.And(l, r) }

    def parExpr = LPar ~ logicalExpr ~ RPar ^^ { case l ~ e ~ r => e }

    def basic = parExpr | notExpr | comparison

    orExpr | andExpr | basic
  }

  def block: Parser[Tree.Expression] = LBracket ~ singleExpr.* ~ RBracket ^^ { case _ ~ e ~ _ => {
    if (e.isEmpty) EmptyExpr
    else if (e.size == 1) e.head
    else Sequence(e)
  } }

  def parseIte: Parser[Tree.Ite] = {

    If ~ LPar ~ logicalExpr ~ RPar ~ singleExpr ~ (Else ~ singleExpr).? ^^ {
      case _ ~ _ ~ log ~ _ ~ thenn ~ Some(_ ~ elze) => Ite(log, thenn, elze)
      case _ ~ _ ~ log ~ _ ~ thenn ~ None => Ite(log, thenn, EmptyExpr)
    }
  }

  def parseWhen: Parser[Tree.When] = {
    When ~ LPar ~ logicalExpr ~ RPar ~ block ^^ {
      case _ ~ _ ~ cond ~ _ ~ act => Tree.When(cond, act)
    }
  }

  def parseDo: Parser[Tree.Do] = {
    Do ~ DoNow.? ~ value ^^ {
      case _ ~ Some(_) ~ v => Tree.Do(v, true)
      case _ ~ None ~ v => Tree.Do(v, false)
    }
  }

  def singleExpr = parseIte | parseDo | block

  def expr: Parser[Tree.Expression] = {
    rep1(singleExpr | parseWhen) ^^ { // When only allowed at top level !
      elems =>
        if (elems.length > 1) Sequence(elems)
        else elems.head
    }
  }

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def rest: Reader[Token] = new TokenReader(tokens.tail)

    override def pos: Position = NoPosition

    override def atEnd: Boolean = tokens.isEmpty
  }

  def apply(tokens: Seq[Token]): Either[CompileError, Tree.Expression] = {
    val reader = new TokenReader(tokens)
    phrase(expr)(reader) match {
      case NoSuccess(msg, next) => Left(CompileError(msg))
      case Success(result, next) => Right(result)
    }
  }

  def parseLogicalExpression(tokens: Seq[Token]): Either[CompileError, Tree.LogicalExpression] = {
    val reader = new TokenReader(tokens)
    phrase(logicalExpr)(reader) match {
      case NoSuccess(msg, next) => Left(CompileError(msg))
      case Success(result, next) => Right(result)
    }
  }
}
