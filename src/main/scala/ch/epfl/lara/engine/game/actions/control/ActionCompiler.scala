package ch.epfl.lara.engine.game.actions.control

import ch.epfl.lara.engine.game.actions.control.compiler.Tokens.Token
import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.actions.control.compiler.{CompileError, Lexer, Parser}

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

  def compileProgram(program: String): (Expression, List[When]) =
    compile(program, Parser.apply) match {
      // parseIte | parseDo | block | when
      case e: When => (EmptyExpr(), e :: Nil)
      case Sequence(list) =>
        val (whens, expr) = list.partition(_.isInstanceOf[When])
        (Sequence(expr), whens.map(_.asInstanceOf[When]))
      case code => (code, Nil)
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
