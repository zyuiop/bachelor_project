package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.{CharacterState, PlayerState}
import ch.epfl.lara.engine.game.items.Inventory

import scala.util.Try

/**
  * @author Louis Vialar
  */
case object ActionStopInteract extends Action with ActionBuilder {
  override def apply(inState: CharacterState): Int = {

    val inventory = inState.currentOpenInventory

    if (inventory.isEmpty) {
      // Try to leave interact
      if (!inState.isInstanceOf[PlayerState]) {
        throw new IllegalStateException("This command can only be ran by real players")
      }

      val player = inState.asInstanceOf[PlayerState]

      if (player.controlled.isEmpty) {
        println("You don't control anyone!")
        0
      } else {
        println(s"You finally release your powers and leave the body of ${player.controlled.get.name}...")
        player.release()
        5
      }
      0
    } else {
      inventory.get.printClose(inState.ps)
      inState.stopInteracting()
      3
    }
  }

  override val triggeringKeywords: Set[String] = Set("leave", "quit", "exit", "back", "go", "close", "release")

  override def apply(input: Array[String]): Try[Action] = Try {
    this
  }
}
