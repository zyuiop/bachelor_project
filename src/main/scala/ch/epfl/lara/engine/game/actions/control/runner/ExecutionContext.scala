package ch.epfl.lara.engine.game.actions.control.runner

import ch.epfl.lara.engine.game.actions.ActionParser
import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}
import ch.epfl.lara.engine.game.scheduler.Scheduler
import ch.epfl.lara.engine.game.{CharacterState, GameState}

import scala.collection.mutable
import scala.util.Try

/**
  * @author Louis Vialar
  */
class ExecutionContext(program: Expression, triggers: List[When], entity: CharacterState) extends MessageHandler {
  private var currentTime = GameState.scheduler.currentTime

  private def env(additionnal: Map[String, Environment] = Map()): Environment = MapEnvironment(
    // Shortcuts for self attributes
    entity.attributes.mapValues(ValueEnvironment) ++
      // Actual env, overrides self attributes if same name
      Map(
        "time" -> ValueEnvironment(Scheduler.timeToDayTime(currentTime).toString),
        "totalTime" -> ValueEnvironment(currentTime.toString),
        "characters" -> PassByNameEnvironment(() => MapEnvironment(
          GameState.registry.getEntities(entity.currentRoom).map(state => (state.name, ObjectMappingEnvironment(state)))
            .toMap + ("me" -> ObjectMappingEnvironment(entity)) + ("player" -> ObjectMappingEnvironment(GameState.registry.player))
        )),
        "room" -> PassByNameEnvironment(() => ObjectMappingEnvironment(entity.currentRoom)),
        "state" -> PassByNameEnvironment(() => ObjectMappingEnvironment(GameState))
      ) ++ additionnal)

  case class BranchState(queue: mutable.Queue[Expression], var nextRun: Int, var moreEnv: Map[String, Environment])

  private var currentState = BranchState(mutable.Queue(), 0, Map())

  private var branches = List[BranchState]()
  private var stopped = true
  private var scheduled = false

  def runNow = runTick(GameState.scheduler.currentTime)

  private def runTick(currentTick: Int): Unit = {
    if (stopped)
      return

    if (currentState.nextRun > 0) {
      currentState.nextRun -= 1 // Will run every tick
    }

    currentTime = currentTick

    // Check the triggers every tick
    checkTriggers()(env())

    while (currentState.nextRun <= 0) {
      while (currentState.queue.isEmpty) next()

      if (currentState.nextRun <= 0)
        execute(this.currentState.queue.dequeue)
    }
  }

  def start(): Unit = {
    stopped = false
    if (scheduled) return

    GameState.scheduler.runRegular(1, 1)(runTick)
    scheduled = true
  }

  def stop() = {
    stopped = true
  }

  private def schedule(expr: Expression) = {
    currentState.queue.enqueue(expr)
  }

  private def interrupt(moreEnv: Map[String, Environment] = Map()) = {
    branches = currentState :: branches
    currentState = BranchState(mutable.Queue(), 0, moreEnv)
  }

  /**
    * Refils the running queue with the last paused branch, or the initial program
    */
  private def next() = {
    if (branches.nonEmpty) {
      currentState = branches.head
      branches = branches.tail
    } else {
      currentState.queue.enqueue(program)
    }
  }

  private def suspendFor(time: Int) = {
    currentState.nextRun = time
  }

  private def execute(expr: Expression) = {
    implicit val env: Environment = this.env(currentState.moreEnv)

    expr match {
      case Ite(cond, left, right) =>
        interrupt()

        schedule(if (valueAsBoolean(resolve(cond))) left else right)
      case Do(act, immediate) =>
        // Compile action
        val action = ActionParser.DefaultParser(resolve(act).asString.split(" ")).get

        val time = action.execute(entity)

        if (!immediate)
          suspendFor(time)
      case Set(field, value) =>
        val path = field.parts
        if (path.length == 1 || (path.length == 3 && path.head == "characters" && path(2) == "attributes")) {
          val key = path.last
          val entity: CharacterState = if (path.length > 1) path(1) match {
            case "me" => this.entity
            case "player" => GameState.registry.player
            case other => GameState.registry.getEntities(this.entity.currentRoom).find(_.name == other).get
          } else this.entity // Shortcut

          entity.changeAttribute(key, resolve(value) match {
            case NullValue => null
            case other => other.asString
          })
        } else {
          throw new UnsupportedOperationException("cannot modify " + path)
        }

      case Sequence(exprs) =>
        exprs.foreach(schedule)
      case EmptyExpr() =>
    }
  }

  private def checkTriggers(moreEnv: Map[String, Environment] = Map())(implicit env: Environment): Unit = {
    if (stopped)
      return

    triggers.foreach(t => {
      if (valueAsBoolean(resolve(t.cond))) {
        interrupt(moreEnv)
        schedule(t.when)
      }
    })
  }

  def handle(message: Message) = {
    val moreEnv = Map(
      "trigger" -> ObjectMappingEnvironment(message)
    )
    implicit val env: Environment = this.env(moreEnv)

    checkTriggers(moreEnv)
  }

  private def valueAsBoolean(value: TypedValue[_]) = value match {
    case BooleanValue(b) => b
    case t@UnknownTypeValue(v) if t.canBeBoolean => v.toBoolean
    case _ => throw new UnsupportedOperationException("unsupported boolean operation on " + value)
  }

  private def resolveOp(operation: Operation)(implicit env: Environment): TypedValue[_] = {
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

  private def resolve(value: Value)(implicit env: Environment): TypedValue[_] = value match {
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

  private def tryResolve(value: Value)(implicit env: Environment) = Try(resolve(value))

  private def resolveAsNumber(value: Value)(implicit env: Environment): Int = resolve(value) match {
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
