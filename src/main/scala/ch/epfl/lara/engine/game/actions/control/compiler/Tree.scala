package ch.epfl.lara.engine.game.actions.control.compiler

/**
  * @author Louis Vialar
  */
object Tree {

  sealed trait Expression

  case class Ite(cond: LogicalExpression, thenn: Expression, elze: Expression) extends Expression

  case class When(cond: LogicalExpression, when: Expression) extends Expression

  case class Do(what: Value, immediate: Boolean) extends Expression

  case class Sequence(list: List[Expression]) extends Expression

  case object EmptyExpr extends Expression

  sealed trait LogicalExpression

  case class And(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

  case class Or(left: LogicalExpression, right: LogicalExpression) extends LogicalExpression

  case class Not(e: LogicalExpression) extends LogicalExpression

  sealed trait Value

  case class Identifier(parts: List[String]) extends Value

  case class Concat(left: Value, right: Value) extends Value

  sealed trait Literal extends Value

  case class IntLiteral(value: Int) extends Literal

  case class StringLiteral(value: String) extends Literal

  case class BooleanLiteral(value: Boolean) extends Literal with Comparison



  sealed trait Comparison extends LogicalExpression

  case class Eq(left: Value, right: Value) extends Comparison

  case class Neq(left: Value, right: Value) extends Comparison

  case class Lte(left: Value, right: Value) extends Comparison

  case class Lt(left: Value, right: Value) extends Comparison

  case class Ht(left: Value, right: Value) extends Comparison

  case class Hte(left: Value, right: Value) extends Comparison

  case class In(left: Value, right: Value) extends Comparison

}
