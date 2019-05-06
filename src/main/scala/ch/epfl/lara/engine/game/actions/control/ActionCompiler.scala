package ch.epfl.lara.engine.game.actions.control

import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.actions.control.compiler.{CompileError, Lexer, Parser}

/**
  * Compiles a list of actions for a NPC
  *
  * @author Louis Vialar
  */
object ActionCompiler {
  def compileProgram(program: String): (Expression, List[When]) = {
    val tokens = Lexer(program)

    if (tokens.isRight) {
      Parser(tokens.right.get) match {
        case Right(code) => code match {
          // parseIte | parseDo | block | when
          case e: When => (EmptyExpr(), e :: Nil)
          case Sequence(list) =>
            val (expr, whens) = list.partition {
              case e: When => false
              case _ => true
            }

            (Sequence(expr), whens.map(_.asInstanceOf[When]))
          case _ => (code, Nil)
        }
        case Left(err) =>
          prettyPrint("parser", err, program)
      }
    }
    else {
      prettyPrint("lexer", tokens.left.get, program)
    }
  }

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
