package ch.epfl.lara.engine.game.items.interactables

import java.io.PrintStream

import ch.epfl.lara.engine.game.items.{ComplexInteractable, Inventory, InventoryInterceptor, Storable}

/**
  * @author Louis Vialar
  */
class InventoryHolderItem(name: String, initialItems: Map[Storable, Int]) extends Inventory(initialItems, name) with ComplexInteractable with InventoryInterceptor {
  override def printContent(implicit printStream: PrintStream): Unit = {
    printStream.println(s"In the $name you find:")
    super.printContent
  }

  override def printOpen(implicit ps: PrintStream): Unit = {
    ps.println(s"You open the $name. It contains: ")
    super.printContent
  }

  override def printClose(implicit ps: PrintStream): Unit = super[InventoryLike].printClose


  /**
    * The name under which this item can be referenced from the command line
    */
  override val displayName: String = name.toLowerCase
}