package ch.epfl.lara.engine.game.items.mutable

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder, ActionParser}
import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.items.{Item, Pickable}
import ch.epfl.lara.engine.game.{Inventory, PlayerState, MutableInventoryImpl}

import scala.util.Try

/**
  * @author Louis Vialar
  */
class InventoryHolderItem(val name: String, initialItems: Inventory) extends Item with Interactable with Inventory {
  private lazy val completeActionParser: ActionParser = ActionParser(leaveInventoryAction, inventory.actionParser)

  private val leaveInventoryAction: ActionBuilder[Action] = new ActionBuilder[Action] {
    private val action: Action = (state, out) => {
      out.println(s"You close the $name")
      state.dequeueParser()
    }

    override def apply(input: Array[String]): Try[Action] = Try(action)

    override val triggeringKeywords: Set[String] = Set("leave", "quit", "exit", "back", "go", "close")
  }

  override def actionParser: ActionParser = completeActionParser

  private val inventory = new MutableInventoryImpl(initialItems.getContent) {
    override def printContent(implicit printStream: PrintStream): Unit = {
      printStream.println(s"In the $name you find:")
      super.printContent
    }
  }

  /**
    * Computes the result of the player interacting with this entity
    *
    * @param state the source state of the level
    * @return the new state of the scene, as well as the updated version of this interactable
    */
  override def interact(state: PlayerState)(implicit out: PrintStream): PlayerState = {
    out.println(s"You open the $name. It contains: ")
    super.printContent
    state.addParser(actionParser)
  }

  override def take(o: Pickable, quantity: Int): Inventory = inventory.take(o, quantity)

  override def add(o: Pickable, quantity: Int): Inventory = inventory.add(o, quantity)

  override def canTake(o: Pickable, quantity: Int): Boolean = inventory.canTake(o, quantity)

  override def getContent: Map[Pickable, Int] = inventory.getContent

  /**
    * The name under which this item can be referenced from the command line
    */
  override val displayName: String = name.toLowerCase
}