package ch.epfl.lara.engine.game.actions.control.runner

import ch.epfl.lara.engine.game.actions.ActionParser
import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}
import ch.epfl.lara.engine.game.{CharacterState, GameState}

import scala.collection.immutable.Queue
import scala.util.Try

/**
  * @author Louis Vialar
  */
class ExecutionContext(program: Expression, triggers: List[When], entity: CharacterState) extends MessageHandler {
  private def env(additionnal: Map[String, Environment] = Map()): Environment = MapEnvironment(Map(
    "time" -> ValueEnvironment(GameState.scheduler.dayTime.toString),
    "totalTime" -> ValueEnvironment(GameState.scheduler.currentTime.toString),
    "characters" -> PassByNameEnvironment(() => MapEnvironment(
      GameState.registry.getEntities(entity.currentRoom).map(state => (state.name, ObjectMappingEnvironment(state)))
        .toMap + ("me" -> ObjectMappingEnvironment(entity)) + ("player" -> ObjectMappingEnvironment(GameState.registry.player))
    )),
    "room" -> PassByNameEnvironment(() => ObjectMappingEnvironment(entity.currentRoom)),
    "state" -> PassByNameEnvironment(() => ObjectMappingEnvironment(GameState)),
    "trigger" -> MapEnvironment(Map("__name" -> ValueEnvironment("None")))
  ) ++ additionnal)

  private var queue = Queue[Expression]()
  private var branches = Queue[(Int, Queue[Expression])]()
  private var nextRun = 0

  private def runTick(currentTick: Int, expectedTick: Int): Unit = {
    if (nextRun > 0) {
      nextRun -= 1 // Will run every tick
    }

    // Check the triggers every tick
    checkTriggers(env())

    while (nextRun <= 0) {
      if (this.queue.isEmpty) next()

      val (n, queue) = this.queue.dequeue
      this.queue = queue
      execute(n)
    }
  }

  def start() = {
    GameState.scheduler.runRegular(1, 1)(runTick)
  }

  private def schedule(expr: Expression) = {
    queue.enqueue(expr)
  }

  private def interrupt() = {
    branches = branches.enqueue((nextRun, queue))
    queue = Queue()
    nextRun = 0
  }

  /**
    * Refils the running queue with the last paused branch, or the initial program
    */
  private def next() = {
    if (branches.nonEmpty) {
      ((nextRun, queue), branches) = branches.dequeue
    } else {
      queue = queue.enqueue(program)
    }
  }

  private def suspendFor(time: Int) = {
    nextRun = time
  }

  private def execute(expr: Expression) = {
    implicit val env: Environment = this.env()

    expr match {
      case Ite(cond, left, right) =>
        if (runCondition(cond)) schedule(left)
        else schedule(right)
      case Do(act, immediate) =>
        // Compile action
        val action = ActionParser.DefaultParser(resolve(act).asString.split(" ")).get

        val time = action.execute(entity)

        if (!immediate)
          suspendFor(time)

        // Check the triggers once we did something (no need for conditional branches)
        checkTriggers
      case Sequence(exprs) =>
        exprs.foreach(schedule)
      case EmptyExpr() =>
    }
  }

  private def checkTriggers(implicit env: Environment) = {
    triggers.foreach(t => {
      if (runCondition(t.cond)) {
        interrupt()
        schedule(t.when)
      }
    })
  }

  def handle(message: Message) = {
    implicit val env: Environment = this.env(Map(
      "trigger" -> ObjectMappingEnvironment(message)
    ))

    checkTriggers
  }

  private def runCondition(cond: LogicalExpression)(implicit env: Environment): Boolean = {
    cond match {
      case And(l, r) => runCondition(l) && runCondition(r)
      case Or(l, r) => runCondition(l) || runCondition(r)
      case Not(e) => !runCondition(e)
      case c: Comparison => checkComparison(c)
    }
  }


  def resolve(value: Value)(implicit env: Environment): TypedValue[_] = value match {
    case Concat(left, right) => (resolve(left), resolve(right)) match {
      case (SetValue(l1), SetValue(l2)) => SetValue(l1 ++ l2)
      case (SetValue(lst), v) => SetValue(lst + v.asString)
      case (v, SetValue(lst)) => SetValue(lst + v.asString)
      case (l, r) => StringValue(l.asString + r.asString)
    }
    case StringLiteral(s) => StringValue(s)
    case BooleanLiteral(b) => BooleanValue(b)
    case IntLiteral(i) => IntValue(i)
    case NullLiteral() => NullValue
    case Identifier(parts) =>
      env.resolvePath(parts).get
  }

  def tryResolve(value: Value)(implicit env: Environment) = Try(resolve(value))

  def resolveAsNumber(value: Value)(implicit env: Environment): Int = resolve(value) match {
    case IntValue(i) => i
    case u@UnknownTypeValue(v) if u.canBeInt => v.toInt
    case _ => throw new IllegalArgumentException(value + " cannot be considered as an int!")
  }

  def checkComparison(condition: Comparison)(implicit env: Environment): Boolean = condition match {
    case BooleanLiteral(value) => value
    case Eq(left: Value, right: Value) =>
      val (l, r) = (tryResolve(left), tryResolve(right))

      if (l.isFailure) {
        r.isSuccess && r.get.value == NullValue
      } else if (r.isFailure) {
        l.isSuccess && l.get.value == NullValue
      } else {
        l.get.asString == r.get.asString
      }
    case Neq(left: Value, right: Value) =>
      !checkComparison(Eq(left, right))
    case Lte(left: Value, right: Value) =>
      resolveAsNumber(left) <= resolveAsNumber(right)
    case Lt(left: Value, right: Value) =>
      resolveAsNumber(left) < resolveAsNumber(right)
    case Ht(left: Value, right: Value) =>
      resolveAsNumber(left) > resolveAsNumber(right)
    case Hte(left: Value, right: Value) =>
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
