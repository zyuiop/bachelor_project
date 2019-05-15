package ch.epfl.lara.engine.game.control.runner

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.actions.ActionParser
import ch.epfl.lara.engine.game.control.compiler.Tree._
import ch.epfl.lara.engine.game.entities.CharacterState
import ch.epfl.lara.engine.game.messaging.{Message, MessageHandler}
import ch.epfl.lara.engine.game.scheduler.Scheduler

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
class CharacterExecutionContext(program: Expression, triggers: List[When], interrupts: List[On], entity: CharacterState) extends BaseExecutionContext with MessageHandler {
  private var currentTime = 0

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
        "state" -> PassByNameEnvironment(() => ObjectMappingEnvironment(GameState.get))
      ) ++ additionnal)

  case class BranchState(stack: mutable.ArrayStack[Expression], var nextRun: Int, var moreEnv: Map[String, Environment])

  private var currentState = BranchState(mutable.ArrayStack(program), 0, Map())

  private val interruptMap = interrupts.flatMap(on => on.conds.parts.map(part => (part, on))).groupBy(_._1).mapValues(_.sortBy(_._2.priority).map(_._2.doo))

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
    triggers.foreach(t => {
      if (valueAsBoolean(resolve(t.cond)(env()))) {
        interrupt()
        schedule(t.when)
      }
    })

    // Run the rest of the tick
    while (currentState.nextRun <= 0) {
      while (currentState.stack.isEmpty && branches.nonEmpty) next()

      if (currentState.stack.isEmpty && branches.isEmpty)
        return // Nothing to run


      if (currentState.nextRun <= 0)
        execute(this.currentState.stack.pop())
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
    currentState.stack.push(expr)
  }

  private def interrupt(moreEnv: Map[String, Environment] = Map()) = {
    branches = currentState :: branches
    currentState = BranchState(mutable.ArrayStack(), 0, moreEnv)
  }

  /**
    * Refils the running queue with the last paused branch, or the initial program
    */
  private def next() = {
    if (branches.nonEmpty) {
      currentState = branches.head
      branches = branches.tail
    }
  }

  private def suspendFor(time: Int) = {
    currentState.nextRun = time
  }

  private def execute(expr: Expression) = {
    implicit val env: Environment = this.env(currentState.moreEnv)

    expr match {
      case Ite(cond, left, right) =>
        schedule(if (valueAsBoolean(resolve(cond))) left else right)
      case While(cond, act) =>
        if (valueAsBoolean(resolve(cond))) {
          // Re-schedule the while
          schedule(expr)

          // Schedule the action
          schedule(act)
        }
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
        // Add all expressions to the stack
        // Reverse the expressions as we need to add the last expression first (LIFO stack)
        exprs.reverse.foreach(schedule)
      case EmptyExpr() =>
    }
  }

  def handle(message: Message): Unit = {
    if (stopped)
      return

    val interrupts = interruptMap.getOrElse(message.getClass.getSimpleName, Nil)

    if (interrupts.nonEmpty) {
      val moreEnv = Map(
        "trigger" -> ObjectMappingEnvironment(message)
      )

      interrupts.foreach(i => {
        interrupt(moreEnv)
        schedule(i)
      })
    }
  }

}
