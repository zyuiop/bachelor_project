package ch.epfl.lara.engine.game.actions.control.runner

import ch.epfl.lara.engine.game.actions.ActionParser
import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}
import ch.epfl.lara.engine.game.{CharacterState, GameState}

import scala.collection.immutable.Queue
import scala.collection.mutable
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
    "state" -> PassByNameEnvironment(() => ObjectMappingEnvironment(GameState))
  ) ++ additionnal)

  case class BranchState(queue: mutable.Queue[Expression], var nextRun: Int, var moreEnv: Map[String, Environment])

  private var currentState = BranchState(mutable.Queue(), 0, Map())

  private var branches = Queue[BranchState]()
  private var stopped = true
  private var scheduled = false

  private def runTick(currentTick: Int, expectedTick: Int): Unit = {
    if (stopped)
      return


    if (currentState.nextRun > 0) {
      currentState.nextRun -= 1 // Will run every tick
    }

    // Check the triggers every tick
    checkTriggers()(env())

    while (currentState.nextRun <= 0) {
      while (currentState.queue.isEmpty) next()

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
    branches = branches.enqueue(currentState)
    currentState = BranchState(mutable.Queue(), 0, moreEnv)
  }

  /**
    * Refils the running queue with the last paused branch, or the initial program
    */
  private def next() = {
    if (branches.nonEmpty) {
      val (state, nbranches) = branches.dequeue
      currentState = state
      branches = nbranches
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
        if (runCondition(cond)) schedule(left)
        else schedule(right)
      case Do(act, immediate) =>
        // Compile action
        val action = ActionParser.DefaultParser(resolve(act).asString.split(" ")).get

        val time = action.execute(entity)

        if (!immediate)
          suspendFor(time)

        // Check the triggers once we did something (no need for conditional branches)
        // We don't reuse the previous env as it's outdated
        checkTriggers()(this.env())
      case Sequence(exprs) =>
        exprs.foreach(schedule)
      case EmptyExpr() =>
    }
  }

  private def checkTriggers(moreEnv: Map[String, Environment] = Map())(implicit env: Environment): Unit = {
    if (stopped)
      return

    triggers.foreach(t => {
      if (runCondition(t.cond)) {
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
