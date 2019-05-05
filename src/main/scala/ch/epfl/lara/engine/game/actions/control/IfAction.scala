package ch.epfl.lara.engine.game.actions.control

import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.actions.Action
import ch.epfl.lara.engine.game.actions.control.compiler.Tree.LogicalExpression

/**
  * @author Louis Vialar
  */
case class IfAction(cond: LogicalExpression, actions: List[Action]) extends Action {
  override def apply(v1: CharacterState): Int = throw new IllegalArgumentException("cannot run if action")
}
