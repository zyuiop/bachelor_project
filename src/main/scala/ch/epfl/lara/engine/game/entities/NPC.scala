package ch.epfl.lara.engine.game.entities

import java.io.PrintStream

import ch.epfl.lara.engine.game.scheduler.Schedulable
import ch.epfl.lara.engine.game.{CharacterState, GameState}

import scala.collection.mutable

/**
  * @author Louis Vialar
  */

// TODO: make it able to react to messages directly
class NPC(startState: CharacterState, program: String) extends CharacterState(
  startState.currentRoom, startState.currentPosition, startState.name,
  startState.inventory, startState.attributes.toMap,
  startState.currentParser, new PrintStream(_ => ())
) {

  private val commands: mutable.Queue[String] = mutable.Queue()

  reset()

  private def reset(): Unit = {
    commands.enqueue(program.split("\n").filterNot(_.isEmpty): _*)
  }

  def schedulable(n: Int = GameState.scheduler.currentTime): Schedulable = new Schedulable {
    override val nextRun: Int = n

    override def run(tick: Int): Option[Schedulable] = {
      Some(runNextCommand(n))
    }
  }

  private def runNextCommand(tick: Int): Schedulable = {
    if (commands.isEmpty) reset()


    val com = commands.dequeue()
    val parsed = currentParser.apply(com.split(" ")).toOption
    if (parsed.isEmpty) schedulable(1 + tick) // ignore
    else {
      val t = parsed.get.apply(this)
      schedulable(t + tick)
    }
  }

  override def spawn(): Unit = {
    super.spawn()
    GameState.scheduler.schedule(schedulable())
  }
}
