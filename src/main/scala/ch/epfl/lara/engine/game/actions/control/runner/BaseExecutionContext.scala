package ch.epfl.lara.engine.game.actions.control.runner

import ch.epfl.lara.engine.game.actions.control.compiler.Tree._

import scala.util.Try

/**
  * @author Louis Vialar
  */
class BaseExecutionContext {
  protected def valueAsBoolean(value: TypedValue[_]): Boolean = value match {
    case BooleanValue(b) => b
    case t@UnknownTypeValue(v) if t.canBeBoolean => v.toBoolean
    case _ => throw new UnsupportedOperationException("unsupported boolean operation on " + value)
  }

  protected def resolveOp(operation: Operation)(implicit env: Environment): TypedValue[_] = {
    val (left, right) = (() => resolve(operation.left), () => resolve(operation.right))

    def numericOp(comb: (Int, Int) => Int) = (left(), right()) match {
      case (IntValue(l), IntValue(r)) => IntValue(comb(l, r))
      case (l@UnknownTypeValue(lv), IntValue(r)) if l.canBeInt => IntValue(comb(lv.toInt, r))
      case (IntValue(l), r@UnknownTypeValue(rv)) if r.canBeInt => IntValue(comb(rv.toInt, l))
      case _ => throw new UnsupportedOperationException("unsupported int operation on " + left + " and " + right)
    }

    def booleanOp(comb: (() => Boolean, () => Boolean) => Boolean) = {
      BooleanValue(comb(() => valueAsBoolean(left()), () => valueAsBoolean(right())))
    }

    def setOrNumericOp(setOp: (collection.Set[String], collection.Set[String]) => collection.Set[String], stringOp: (String, String) => String, comb: (Int, Int) => Int): TypedValue[_] = (left(), right()) match {
      case (SetValue(l1), SetValue(l2)) => SetValue(setOp(l1, l2))
      case (SetValue(lst), v) => SetValue(setOp(lst, Predef.Set(v.asString)))
      case (v, SetValue(lst)) => SetValue(setOp(lst, Predef.Set(v.asString)))
      case (IntValue(l), IntValue(r)) => IntValue(comb(l, r))
      case (l@UnknownTypeValue(lv), IntValue(r)) if l.canBeInt => IntValue(comb(lv.toInt, r))
      case (IntValue(l), r@UnknownTypeValue(rv)) if r.canBeInt => IntValue(comb(rv.toInt, l))
      case (l, r) => StringValue(stringOp(l.asString, r.asString))
    }

    operation match {
      case s: Sum => setOrNumericOp(_ ++ _, _ + _, _ + _)
      case d: Difference => setOrNumericOp(_ -- _, _ replaceAll(_, ""), _ - _)
      case m: Multiplication => numericOp(_ * _)
      case d: Division => numericOp(_ / _)
      case m: Module => numericOp(_ % _)
      case _: And => booleanOp(_ () && _ ())
      case _: Or => booleanOp(_ () || _ ())
    }
  }

  protected def resolve(value: Value)(implicit env: Environment): TypedValue[_] = {
    value match {
      case op: Operation => resolveOp(op)
      case c: Comparison => BooleanValue(checkComparison(c))
      case Not(n) => BooleanValue(!valueAsBoolean(resolve(n)))
      case StringLiteral(s) => StringValue(s)
      case BooleanLiteral(b) => BooleanValue(b)
      case IntLiteral(i) => IntValue(i)
      case NullLiteral() => NullValue
      case Identifier(parts) =>
        env.resolvePath(parts).get
    }
  }

  private def tryResolve(value: Value)(implicit env: Environment) = Try(resolve(value))

  protected def resolveAsNumber(value: Value)(implicit env: Environment): Int = resolve(value) match {
    case IntValue(i) => i
    case u@UnknownTypeValue(v) if u.canBeInt => v.toInt
    case _ => throw new IllegalArgumentException(value + " cannot be considered as an int!")
  }

  def checkComparison(condition: Comparison)(implicit env: Environment): Boolean = condition match {
    case Eq(left: Value, right: Value) =>
      val (l, r) = (tryResolve(left), tryResolve(right))

      if (l.isFailure) {
        r.isSuccess && r.get == NullValue
      } else if (r.isFailure) {
        l.isSuccess && l.get == NullValue
      } else {
        l.get.asString == r.get.asString
      }
    case Neq(left: Value, right: Value) =>
      !checkComparison(Eq(left, right))
    case Lte(left: Value, right: Value) =>
      resolveAsNumber(left) <= resolveAsNumber(right)
    case Lt(left: Value, right: Value) =>
      resolveAsNumber(left) < resolveAsNumber(right)
    case Gt(left: Value, right: Value) =>
      resolveAsNumber(left) > resolveAsNumber(right)
    case Gte(left: Value, right: Value) =>
      resolveAsNumber(left) >= resolveAsNumber(right)
    case In(left: Value, right: Value) =>
      val l = resolve(left)
      val r = resolve(right)

      r match {
        case StringValue(_) | UnknownTypeValue(_) =>
          l match {
            case StringValue(_) | UnknownTypeValue(_) =>
              r.asString.contains(l.asString)

            case _ =>
              throw new IllegalArgumentException(left + " cannot be at the left hand side of a `in` operator if the right hand is a string")
          }

        case SetValue(rightSet) =>

          l match {
            case SetValue(leftSet) => leftSet.forall(s => rightSet(s))
            case _ => rightSet(l.asString)
          }
        case _ => throw new IllegalArgumentException("Illegal use of in operator with right hand " + r)
      }
  }
}
