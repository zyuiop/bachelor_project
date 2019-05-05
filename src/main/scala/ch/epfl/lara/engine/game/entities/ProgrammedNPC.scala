package ch.epfl.lara.engine.game.entities

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.Action
import ch.epfl.lara.engine.game.actions.control.compiler.Tree.Expr
import ch.epfl.lara.engine.game.actions.control.{ActionCompiler, ConditionRunner, IfAction}
import ch.epfl.lara.engine.game.messaging.Message
import ch.epfl.lara.engine.game.scheduler.Schedulable
import ch.epfl.lara.engine.game.{CharacterState, GameState}

import scala.collection.mutable

/**
  * @author Louis Vialar
  */

class ProgrammedNPC(startState: CharacterState,
                    program: String = "",
                    triggers: List[(String, String)] = Nil) extends CharacterState(
  startState.currentRoom, startState.currentPosition, startState.name,
  startState.inventory.getContent, startState.attributes.toMap, new PrintStream(_ => ())
) with NPC {

  private val compiledProgram: List[Action] = ActionCompiler.compile(program.split("\n").toList)
  private val compiledTriggers: List[(Expr, List[Action])] = triggers.map {
    case (when, what) => (ActionCompiler.compileCondition(when), ActionCompiler.compile(what.split("\n").toList))
  }

  private val returnTo: mutable.ArrayStack[mutable.Queue[Action]] = mutable.ArrayStack()
  private val commands: mutable.Queue[Action] = mutable.Queue()

  reset()

  private def reset(): Unit = {
    if (returnTo.nonEmpty) {
      commands.enqueue(returnTo.pop(): _*)
    } else commands.enqueue(compiledProgram: _*)
  }

  protected def schedulable(n: Int = GameState.scheduler.currentTime): Schedulable = new Schedulable {
    override val nextRun: Int = n

    override def run(tick: Int): Option[Schedulable] = {
      runTriggers(None)

      Option(runNextCommand(n))
    }
  }

  protected def runNextCommand(tick: Int): Schedulable = {
    while (commands.isEmpty) reset()

    commands.dequeue() match {
      case IfAction(cond, actions) =>
        if (ConditionRunner.runCondition(cond)(this, None)) {
          returnTo.push(commands.clone())
          commands.clear()
          commands.enqueue(actions: _*)
        }

        runNextCommand(tick)
      case action: Action =>
        val t = action.apply(this)
        schedulable(t + tick)
    }
  }

  protected def runTriggers(implicit trigger: Option[Message]): Unit = {
    implicit val me: CharacterState = this

    val triggers = compiledTriggers.filter(pair => ConditionRunner.runCondition(pair._1))
      .map(_._2)
      .map(list => mutable.Queue(list: _*))

    if (triggers.nonEmpty) {
      returnTo.push(commands.clone())
      returnTo ++= triggers
      commands.clear()
      reset()
    }
  }

  override def spawn(): Unit = {
    super.spawn()
    GameState.scheduler.schedule(schedulable())
  }

  override def handle(message: Message): Unit = {
    super.handle(message)

    runTriggers(Some(message))
  }
}
