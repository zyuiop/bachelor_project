package ch.epfl.lara.engine.game

import java.io.{File, PrintStream}

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.data.LevelParser
import ch.epfl.lara.engine.game.entities.CharacterState

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * @author Louis Vialar
  */
object Game {

  private var running: Boolean = true


  private implicit val printStream: PrintStream = Console.out

  val emptyStream = new PrintStream((b: Int) => ())

  def main(args: Array[String]): Unit = {
    val descriptor = LevelParser.readLevel(new File("data/level-1/"))

    val state = descriptor.startLevel()

    loop(state)
  }

  private val systemActionsParser = ActionParser(
    ActionSaveGame // TODO: add quit, ...
  )

  def saveGame(state: CharacterState) = ???

  def loadGame(): CharacterState = ???

  def quitGame(): Unit = {
    running = false
  }

  val parser: ActionParser = ActionParser.DefaultParser union systemActionsParser

  @tailrec
  def loop(state: CharacterState): Unit = {
    if (!running) {
      printStream.println("Good bye!")
      return
    }

    if (GameState.get.isLevelComplete) {
      printStream.println(GameState.get.levelData.data.endText)
      printStream.println("Level success!")
      Thread.sleep(1000)
      // TODO: load next level
      return
    } else if (GameState.get.isLevelFailed) {
      printStream.println("Level failed...")
      Thread.sleep(1000)
      GameState.get.levelData.startLevel()
    }

    // Run action
    val nextStep = StdIn.readLine("> ").split(" ")
    val action = parser(nextStep)

    if (action.isSuccess) {
      val time = action.get.execute(state)
      GameState.get.scheduler.addTime(time)
      loop(state)
    } else {
      println(action.failed.get.getMessage)
      loop(state)
    }
  }
}
