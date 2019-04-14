package ch.epfl.lara.engine.game.items.mutable

import java.io.PrintStream

import ch.epfl.lara.engine.game.decisions.{Action, ActionBuilder, ActionParser}
import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.items.{Item, Pickable}
import ch.epfl.lara.engine.game.{Inventory, LevelState, MutableInventoryImpl}

import scala.util.Try

/**
  * @author Louis Vialar
  */
class InventoryHolderItem(val name: String, initialItems: Inventory) extends Item with Interactable with Inventory {
  private val actionParser: ActionParser = ActionParser {
    InventoryHolderItem.leaveInventoryAction,
    new ActionBuilder[Action] {
      override def apply(input: Array[String]): Try[Action] = ???

      override val triggeringKeywords: Set[String] = Set
    }
  }

  private val inventory = new MutableInventoryImpl(initialItems.getContent)

  /**
    * Computes the result of the player interacting with this entity
    *
    * @param state the source state of the level
    * @return the new state of the scene, as well as the updated version of this interactable
    */
  override def interact(state: LevelState): LevelState = ???

  override def take(o: Pickable, quantity: Int): Inventory = inventory.take(o, quantity)

  override def add(o: Pickable, quantity: Int): Inventory = inventory.add(o, quantity)

  override def canTake(o: Pickable, quantity: Int): Boolean = inventory.canTake(o, quantity)

  override def getContent: Map[Pickable, Int] = inventory.getContent
}

object InventoryHolderItem {
  private val leaveInventoryAction: ActionBuilder[Action] = new ActionBuilder[Action] {
    private val action: Action = new Action {
      // TODO: print message
      override def execute(inState: LevelState)(implicit out: PrintStream): LevelState = inState.dequeueParser()
    }

    override def apply(input: Array[String]): Try[Action] = ???

    override val triggeringKeywords: Set[String] = Set("leave", "quit", "exit", "back", "go", "close")
  }
}
