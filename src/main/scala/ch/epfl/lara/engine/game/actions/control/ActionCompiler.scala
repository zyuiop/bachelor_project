package ch.epfl.lara.engine.game.actions.control

import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.actions.control.compiler.{CompileError, Lexer, Parser}
import ch.epfl.lara.engine.game.actions.{Action, ActionParser}

/**
  * Compiles a list of actions for a NPC
  *
  * @author Louis Vialar
  */
object ActionCompiler {
  def compileProgram(program: String): (Expression, List[When]) = {
    {
      for {
        tokens <- Lexer(program).right
        tree <- Parser(tokens).right
      } yield tree
    } match {
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
      case Left(err) => throw new Exception("compilation of (" + program + ") failed: \n" + err)
    }
  }

  def compileCondition(condition: String): LogicalExpression = {
    compile(condition) match {
      case Right(code) => code
      case Left(err) => throw new Exception("compilation of (" + condition + ") failed: \n" + err)
    }
  }

  def compile(condition: String): Either[CompileError, LogicalExpression] = {
    for {
      tokens <- Lexer(condition).right
      tree <- Parser.parseLogicalExpression(tokens).right
    } yield tree
  }


  def compile(commands: List[String]): List[Action] = {
    def compile(commands: List[String], acc: List[Action]): (List[Action], List[String]) = {
      if (commands.isEmpty)
        (acc.reverse, Nil)
      else {
        val head = commands.head.split(" ")
        val tail = commands.tail

        if (head.head.toLowerCase == "end") {
          (acc.reverse, tail)
        } else if (head.head.toLowerCase == "if") {
          val (under, rest) = compile(tail, Nil)

          // Build the action
          val action: Action = IfAction(compileCondition(head.tail.mkString(" ")), under)

          // Compile the rest
          compile(rest, action :: acc)
        } else if (head.head.toLowerCase == "immediate") {
          compile(tail, new Action {
            override def apply(v1: CharacterState): Int = {
              ActionParser.DefaultParser(head.tail).get.apply(v1)
              0
            }
          } :: acc)
        } else {
          compile(tail, ActionParser.DefaultParser(head).get :: acc)
        }
      }
    }

    val (ret, _) = compile(commands.map(_.trim).filter(_.nonEmpty), Nil)
    ret
  }


}
