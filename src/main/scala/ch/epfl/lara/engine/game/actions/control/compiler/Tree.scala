package ch.epfl.lara.engine.game.actions.control.compiler

/**
  * @author Louis Vialar
  */
object Tree {

  sealed trait Expr

  case class And(left: Expr, right: Expr) extends Expr

  case class Or(left: Expr, right: Expr) extends Expr

  case class Not(e: Expr) extends Expr

  sealed trait Value extends Expr

  case class Identifier(parts: List[String]) extends Value

  case class Concat(left: Value, right: Value) extends Value


  sealed trait Literal extends Value

  case class IntLiteral(value: Int) extends Literal

  case class StringLiteral(value: String) extends Literal

  case class BooleanLiteral(value: Boolean) extends Literal


  sealed trait Comparison extends Expr

  case class Eq(left: Value, right: Value) extends Comparison

  case class Neq(left: Value, right: Value) extends Comparison

  case class Lte(left: Value, right: Value) extends Comparison

  case class Lt(left: Value, right: Value) extends Comparison

  case class Ht(left: Value, right: Value) extends Comparison

  case class Hte(left: Value, right: Value) extends Comparison

  case class In(left: Value, right: Value) extends Comparison

}
