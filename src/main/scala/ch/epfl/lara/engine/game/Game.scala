package ch.epfl.lara.engine.game

import java.io.{File, FileFilter, PrintStream}

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.data.{LevelDescriptor, LevelParser}
import ch.epfl.lara.engine.game.entities.{CharacterState, PlayerState}

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * @author Louis Vialar
  */
object Game {
  println("Loading files...")

  private val levels: Array[LevelDescriptor] = new File("data")
    .listFiles(new FileFilter {
      override def accept(pathname: File): Boolean = pathname.isDirectory && pathname.getName.startsWith("level-")
    })
    .map(file => (file.getName.drop(6).toInt, LevelParser.readLevel(file)))
    .sortBy(_._1)
    .map(_._2)

  println("Done! Loaded " + levels.length + " levels.")

  private var currentLevel = 0

  private def startNextLevel: Option[PlayerState] = {
    currentLevel += 1
    if (levels.length < currentLevel) {
      println()
      println("Congratulations! You completed the game! Why not try it again while following an other path?")
      None
    } else {
      val level = levels(currentLevel - 1)

      Some(level.startLevel())
    }
  }


  private var running: Boolean = true

  private implicit val printStream: PrintStream = Console.out

  def main(args: Array[String]): Unit = {
    startNextLevel.foreach(loop)
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
      val next = startNextLevel
      if (next.nonEmpty)
        loop(next.get)
    } else if (GameState.get.isLevelFailed) {
      printStream.println("Level failed...")
      Thread.sleep(1000)
      loop(GameState.get.levelData.startLevel())
    } else {

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
}
