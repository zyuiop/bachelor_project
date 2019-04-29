package ch.epfl.lara.engine.game.actions.control

import ch.epfl.lara.engine.game.actions.control.ActionCompiler.compile
import ch.epfl.lara.engine.game.actions.control.compiler.{Tokens, Tree}
import ch.epfl.lara.engine.game.actions.control.compiler.Tree.Expr
import ch.epfl.lara.engine.game.actions.{Action, ActionParser}
import ch.epfl.lara.engine.game.actions.control.IfAction

/**
  * Compiles a list of actions for a NPC
  *
  * @author Louis Vialar
  */
object ActionCompiler {
  def compileCondition(condition: String): Expr = {
    val tokens = Tokens.read(condition.trim)
    Tree.extractExpr(tokens)
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
        } else {
          compile(tail, ActionParser.DefaultParser(head).get :: acc)
        }
      }
    }
    val (ret, _) = compile(commands.map(_.trim).filter(_.nonEmpty), Nil)
    ret
  }


}
