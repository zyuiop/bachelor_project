package ch.epfl.lara.engine.game.control.compiler

import ch.epfl.lara.engine.game.control.compiler.Tokens._
import ch.epfl.lara.engine.game.control.compiler.Tree.Expression

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

/**
  * @author Louis Vialar
  */
object Parser extends Parsers {
  override type Elem = Tokens.Token

  def identifier: Parser[Tree.Identifier] = positioned {
    chainl1(positioned {
      accept("identifier", { case Identifier(id) => Tree.Identifier(List(id)) })
    }, Dot() ^^^ { (l: Tree.Identifier, r: Tree.Identifier) =>
      Tree.Identifier(l.parts ::: r.parts)
    })
  }

  def literals: Parser[Tree.Literal] = positioned {
    accept("literal", {
      case BooleanLiteral(b) => Tree.BooleanLiteral(b)
      case StringLiteral(s) => Tree.StringLiteral(s)
      case IntLiteral(i) => Tree.IntLiteral(i)
      case Null() => Tree.NullLiteral()
    })
  }

  def operation: Parser[Tree.Value] = positioned {
    def parValue = (LPar() ~! valueWithOp ~! RPar()) ^^ { case _ ~ v ~ _ => v }

    def notValue = Not() ~! valueWithOp ^^ { case not ~ e => Tree.Not(e) }

    def simpleValue = identifier | literals | parValue | notValue

    def lowestPriorityOp: Parser[Tree.Value] = chainl1(simpleValue, lowestPriorityOp, (Div() | Mod() | Times()) ^^ {
      case Div() => (l: Tree.Value, r: Tree.Value) => Tree.Division(l, r)
      case Mod() => (l: Tree.Value, r: Tree.Value) => Tree.Module(l, r)
      case Times() => (l: Tree.Value, r: Tree.Value) => Tree.Multiplication(l, r)
    })

    def lpOp: Parser[Tree.Value] = chainl1(lowestPriorityOp, lpOp, (Plus() | Minus()) ^^ {
      case Plus() => (l: Tree.Value, r: Tree.Value) => Tree.Sum(l, r)
      case Minus() => (l: Tree.Value, r: Tree.Value) => Tree.Difference(l, r)
    })

    def mpOp: Parser[Tree.Value] = chainl1(lpOp, mpOp, (Eq() | Neq() | Lte() | Lt() | Gte() | Gt() | In()) ^^ {
      case Eq() => (l: Tree.Value, r: Tree.Value) => Tree.Eq(l, r)
      case Neq() => (l: Tree.Value, r: Tree.Value) => Tree.Neq(l, r)
      case Lte() => (l: Tree.Value, r: Tree.Value) => Tree.Lte(l, r)
      case Lt() => (l: Tree.Value, r: Tree.Value) => Tree.Lt(l, r)
      case Gte() => (l: Tree.Value, r: Tree.Value) => Tree.Gte(l, r)
      case Gt() => (l: Tree.Value, r: Tree.Value) => Tree.Gt(l, r)
      case In() => (l: Tree.Value, r: Tree.Value) => Tree.In(l, r)
    })

    def hpOp: Parser[Tree.Value] = chainl1(mpOp, hpOp, (Or() | And()) ^^ {
      case Or() => (l: Tree.Value, r: Tree.Value) => Tree.Or(l, r)
      case And() => (l: Tree.Value, r: Tree.Value) => Tree.And(l, r)
    })


    hpOp
  }


  def valueWithOp: Parser[Tree.Value] = positioned(operation)

  def block: Parser[Tree.Expression] = LBracket() ~! singleExpr.* ~! RBracket() ^^ { case _ ~ e ~ _ => {
    if (e.isEmpty) Tree.EmptyExpr()
    else if (e.size == 1) e.head
    else Tree.Sequence(e)
  }
  }

  def parseIte: Parser[Tree.Ite] = positioned {
    If() ~! LPar() ~! valueWithOp ~! RPar() ~! singleExpr ~! (Else() ~! singleExpr).? ^^ {
      case _ ~ _ ~ log ~ _ ~ thenn ~ Some(_ ~ elze) => Tree.Ite(log, thenn, elze)
      case _ ~ _ ~ log ~ _ ~ thenn ~ _ => Tree.Ite(log, thenn, Tree.EmptyExpr())
    }
  }

  def parseWhen: Parser[Tree.When] = positioned {
    When() ~! LPar() ~! valueWithOp ~! RPar() ~! singleExpr ^^ {
      case _ ~ _ ~ cond ~ _ ~ act => Tree.When(cond, act)
    }
  }

  def parseDo: Parser[Tree.Do] = positioned {
    Do() ~! DoNow().? ~! valueWithOp ^^ {
      case _ ~ Some(_) ~ v => Tree.Do(v, true)
      case _ ~ None ~ v => Tree.Do(v, false)
    }
  }

  def parseSet: Parser[Tree.Set] = positioned {
    identifier ~! Set() ~! valueWithOp ^^ {
      case id ~ _ ~ v => Tree.Set(id, v)
    }
  }

  def singleExpr: Parser[Tree.Expression] = positioned(parseIte | parseDo | parseSet | block)

  def expr: Parser[Tree.Expression] = positioned {
    rep1(singleExpr | parseWhen) ^^ { // When only allowed at top level !
      elems =>
        if (elems.length > 1) Tree.Sequence(elems)
        else elems.head
    }
  }

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def rest: Reader[Token] = new TokenReader(tokens.tail)

    override def pos: Position = if (atEnd) NoPosition else first.pos

    override def atEnd: Boolean = tokens.isEmpty
  }

  private def compile[A <: Positional](tokens: Seq[Token], parser: Parser[A]): Either[CompileError, A] = {
    positioned(phrase(parser))(new TokenReader(tokens)) match {
      case NoSuccess(msg, next) => Left(CompileError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def apply(tokens: Seq[Token]): Either[CompileError, Tree.Expression] = compile(tokens, expr)

  def parseValue(tokens: Seq[Token]): Either[CompileError, Tree.Value] = compile(tokens, valueWithOp)
}
