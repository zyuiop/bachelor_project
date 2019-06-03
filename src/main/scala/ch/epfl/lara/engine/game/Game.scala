package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.actions.general._
import ch.epfl.lara.engine.game.data.{LevelParser, LevelsManager}
import ch.epfl.lara.engine.game.entities.PlayerState
import ch.epfl.lara.engine.game.items.interactables.{BookItem, DescriptiveItem, DoorItem, InventoryHolderItem, SwitchItem}
import ch.epfl.lara.engine.game.items.locks.{InvisibleLock, PhysicalLock}

import scala.util.Try

/**
  * @author Louis Vialar
  */
abstract class Game {

  import ch.epfl.lara.engine.game.data.Properties._

  LevelParser.registerItemType("inventory") { props =>
    new InventoryHolderItem(props("name"), props.inventory("inv"))
  }

  LevelParser.registerItemType("switch") { props =>
    val states = props.multiVal("states") // map states.<stateName> = transition to this state
    val transitions = props.prefixed("transitions") // map states.<stateName> = transition to this state
    val time = props.get("time").flatMap(t => Try(t.toInt).toOption).getOrElse(3)

    new SwitchItem(states, transitions, props("id"), props("name"), time)
  }

  LevelParser.registerItemType("descriptive") { props =>
    val time = props.get("time").flatMap(t => Try(t.toInt).toOption).getOrElse(3)

    new DescriptiveItem(props("name"), props("lore"), time)
  }

  LevelParser.registerItemType("door") { props =>
    val name = props("name")
    val desc = props.multiVal("description")
    new DoorItem(name, props("target"), if (desc.isEmpty) List(s"You go through the $name.") else desc)
  }

  LevelParser.registerLockType("condition") { (item, props) =>
    new InvisibleLock(item, props("message"), props("condition"))
  }

  LevelParser.registerLockType("lock") { (item, props) =>
    val name = props("name")
    new PhysicalLock(item, name, props.getOrElse("helper", ""), props("code"),
      props.getOrElse("successMessage", s"You unlocked the $name!"),
      props.getOrElse("failMessage", s"Wrong code! Try again..."),
    )
  }

  LevelParser.registerItemType("book") { props => new BookItem(props("name"), props.prefixed("pages")) }

  ActionsRegistry.registerActions(
    ActionUseDoor,
    ActionInteract,
    ActionWait,
    ActionSay,
    ActionTime,
    ActionControlStop,
    ActionControl,
    ActionGive,
    ActionRequestReply
  )

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

  private val levelsManager = new LevelsManager

  def startGame() = {
    state = levelsManager.startNextLevel.get
  }

  private def checkLevelState(): Unit = {
    if (GameState.get.isLevelComplete) {
      printStream.println(GameState.get.levelData.data.endText)
      printStream.println("Level success!")
      Thread.sleep(1000)
      val next = levelsManager.startNextLevel
      if (next.nonEmpty)
        state = next.get
      else _onFinishGame()
    } else if (GameState.get.isLevelFailed) {
      printStream.println("Level failed...")
      Thread.sleep(1000)
      state = levelsManager.restartLevel
    }
  }

  final def runCommand(command: String): Unit = {
    // Run action
    val action = state.updateParser(parser)(command split " ")

    println(command + "; " + parser + "; " + action)

    if (action.isSuccess) {
      val time = action.get.apply(state)
      GameState.get.scheduler.addTime(time)
    } else {
      printStream.println(action.failed.get.getMessage)
    }

    // Check if level is finished
    checkLevelState
  }

  val parser: ActionParser = ActionsRegistry.actionsParser union systemActionsParser
}
