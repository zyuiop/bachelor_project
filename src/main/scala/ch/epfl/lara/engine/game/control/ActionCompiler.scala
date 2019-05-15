package ch.epfl.lara.engine.game.control

import ch.epfl.lara.engine.game.control.compiler.Tokens.Token
import ch.epfl.lara.engine.game.control.compiler.Tree._
import ch.epfl.lara.engine.game.control.compiler.{CompileError, Lexer, Parser}

/**
  * Compiles a list of actions for a NPC
  *
  * @author Louis Vialar
  */
object ActionCompiler {
  /* parseLogicalExpression(tokens: Seq[Token]): Either[CompileError, Tree.Value] */

  private def compile[A](program: String, parser: List[Token] => Either[CompileError, A]): A = {
    val tokens = Lexer(program)

    if (tokens.isRight) {
      parser(tokens.right.get) match {
        case Right(code) => code
        case Left(err) =>
          prettyPrint("parser", err, program)
      }
    }
    else {
      prettyPrint("lexer", tokens.left.get, program)
    }
  }

  def compileProgram(program: String): (Expression, List[When], List[On]) =
    compile(program, Parser.apply) match {
      // parseIte | parseDo | block | when
      case e: When => (EmptyExpr(), e :: Nil, Nil)
      case e: On => (EmptyExpr(), Nil, e :: Nil)
      case Sequence(list) =>
        val (expr, whens, ons) = list.foldLeft((List.empty[Expression], List.empty[When], List.empty[On])) {
          case ((e, w, o), expression: When) => (e, expression :: w, o)
          case ((e, w, o), expression: On) => (e, w, expression :: o)
          case ((e, w, o), expression: Expression) => (expression :: e, w, o)
        }

        (if (expr.isEmpty) EmptyExpr() else if (expr.size == 1) expr.head else Sequence(expr.reverse), whens, ons)
      case code => (code, Nil, Nil)
    }

  def compileValue(value: String): Value =
    compile(value, Parser.parseValue)

  private def prettyPrint(phase: String, error: CompileError, code: String) = {
    println("----------- Compilation failed at " + phase + " -----------")

    println(error.err)
    println("\tat " + error.location)
    println()
    val line = code.split("\n")(error.location.line - 1)
    val chars = (" " * (error.location.column - 1)) + "^----"
    println("\t" + line)
    println("\t" + chars)
    println()

    throw new Exception("compilation failed")
  }
}
