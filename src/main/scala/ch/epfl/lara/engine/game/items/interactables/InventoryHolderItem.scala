package ch.epfl.lara.engine.game.items.interactables

import java.io.PrintStream

import ch.epfl.lara.engine.game.items.{Interactable, InteractableInventory, Item, Inventory, Pickable}

/**
  * @author Louis Vialar
  */
class InventoryHolderItem(name: String, initialItems: Map[Pickable, Int]) extends Inventory(initialItems, name) with Item with Interactable with InteractableInventory {
  override def printContent(implicit printStream: PrintStream): Unit = {
    printStream.println(s"In the $name you find:")
    super.printContent
  }

  override def printOpen(implicit ps: PrintStream): Unit = {
    ps.println(s"You open the $name. It contains: ")
    super.printContent
  }

  /**
    * The name under which this item can be referenced from the command line
    */
  override val displayName: String = name.toLowerCase
}