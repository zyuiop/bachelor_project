package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.data.LevelsManager
import ch.epfl.lara.engine.game.entities.PlayerState

/**
  * @author Louis Vialar
  */
abstract class Game {
  implicit val printStream: PrintStream

  private var _onFinishGame: () => Unit = () => ()
  private var _onQuitGame: () => Unit = () => ()

  def onFinishGame_=(func: () => Unit): Unit = this._onFinishGame = func

  def onQuitGame_=(func: () => Unit): Unit = this._onQuitGame = func

  def onFinishGame = this._onFinishGame

  def onQuitGame = this._onQuitGame

  private val systemActionsParser = ActionParser(
    ActionSaveGame // TODO: add quit, ...
  )

  def saveGame() = ???

  def loadGame() = ???

  /**
    * Reference to the current player state
    */
  private var state: PlayerState = _

  def startGame() = {
    state = LevelsManager.startNextLevel.get
  }

  private def checkLevelState(): Unit = {
    if (GameState.get.isLevelComplete) {
      printStream.println(GameState.get.levelData.data.endText)
      printStream.println("Level success!")
      Thread.sleep(1000)
      val next = LevelsManager.startNextLevel
      if (next.nonEmpty)
        state = next.get
      else _onFinishGame()
    } else if (GameState.get.isLevelFailed) {
      printStream.println("Level failed...")
      Thread.sleep(1000)
      state = LevelsManager.restartLevel
    }
  }

  final def runCommand(command: String): Unit = {
    // Run action
    val action = parser(command split " ")

    println(command + "; " + parser + "; " + action)

    if (action.isSuccess) {
      val time = action.get.execute(state)
      GameState.get.scheduler.addTime(time)
    } else {
      printStream.println(action.failed.get.getMessage)
    }

    // Check if level is finished
    checkLevelState
  }

  val parser: ActionParser = ActionParser.DefaultParser union systemActionsParser
}
