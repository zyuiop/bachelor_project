package ch.epfl.lara.engine.game.control.compiler

import scala.util.parsing.input.Positional

/**
  * @author Louis Vialar
  */
object Tree {

  sealed trait Expression extends Positional

  case class Ite(cond: Value, thenn: Expression, elze: Expression) extends Expression

  case class While(cond: Value, doo: Expression) extends Expression

  case class When(cond: Value, when: Expression, priority: Int = 0) extends Expression

  case class On(conds: Identifier, doo: Expression, priority: Int = 0) extends Expression

  case class Do(what: Value, immediate: Boolean, blocking: Boolean) extends Expression

  case class Set(field: Identifier, value: Value) extends Expression

  case class Sequence(list: List[Expression]) extends Expression

  case class EmptyExpr() extends Expression


  sealed trait Value extends Positional

  sealed trait Operation extends Value {
    val left: Value
    val right: Value
  }

  case class Identifier(parts: List[String]) extends Value

  case class Sum(left: Value, right: Value) extends Operation
  case class Difference(left: Value, right: Value) extends Operation
  case class Module(left: Value, right: Value) extends Operation
  case class Multiplication(left: Value, right: Value) extends Operation
  case class Division(left: Value, right: Value) extends Operation

  case class And(left: Value, right: Value) extends Operation

  case class Or(left: Value, right: Value) extends Operation

  case class Not(e: Value) extends Value


  sealed trait Literal extends Value

  case class IntLiteral(value: Int) extends Literal

  case class StringLiteral(value: String) extends Literal

  case class BooleanLiteral(value: Boolean) extends Literal

  case class NullLiteral() extends Literal



  sealed trait Comparison extends Value

  case class Eq(left: Value, right: Value) extends Comparison

  case class Neq(left: Value, right: Value) extends Comparison

  case class Lte(left: Value, right: Value) extends Comparison

  case class Lt(left: Value, right: Value) extends Comparison

  case class Gt(left: Value, right: Value) extends Comparison

  case class Gte(left: Value, right: Value) extends Comparison

  case class In(left: Value, right: Value) extends Comparison

}
