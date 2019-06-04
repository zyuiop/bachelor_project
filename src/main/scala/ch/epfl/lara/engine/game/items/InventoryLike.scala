package ch.epfl.lara.engine.game.items

import java.io.PrintStream

import scala.util.{Failure, Success, Try}

/**
  * @author Louis Vialar
  */
trait InventoryLike {
  def nonEmpty: Boolean = getContent.nonEmpty

  def printContent(implicit printStream: PrintStream): Unit = {
    val c = getContent

    if (c.isEmpty) printEmpty
    else for ((item, quantity) <- c)
      printStream.println(s"\t$quantity\t*\t${item.displayName}")
  }

  val name: String

  def printEmpty(implicit ps: PrintStream): Unit = {
    ps.println(s"\tThe $name is empty...")
  }

  def printOpen(implicit ps: PrintStream): Unit = {
    ps.println(s"You open the $name.")
  }

  def printClose(implicit ps: PrintStream): Unit = {
    ps.println(s"You close the $name.")
  }

  def getContent: Map[Storable, Int]

  /**
    * Take an item in this inventory, returning the updated inventory
    *
    * @param o        the item to take
    * @param quantity the amount of this item to take
    * @return the updated inventory
    */
  def take(o: Storable, quantity: Int): InventoryLike

  /**
    * Add an item to this inventory, returning the updated inventory
    *
    * @param o        the item to add
    * @param quantity the amount of this item to add
    * @return the updated inventory
    */
  def add(o: Storable, quantity: Int): InventoryLike

  /**
    * Check if an item can be taken from this inventory
    *
    * @param o        the item to take
    * @param quantity the quantity we wish to take
    * @return true if we can take this quantity of this item
    */
  def canTake(o: Storable, quantity: Int): Boolean

  /**
    * Transfer objects from the current inventory to an other one. This operation returns a triple, containing the
    * success status of the operation, the updated source inventory, and the updated target inventory.
    *
    * @param target   the target inventory
    * @param o        the object to transfer
    * @param quantity the quantity of objects to transfer
    * @return a triple (success, source, target)
    */
  def transferTo(target: InventoryLike, o: Storable, quantity: Int): (Boolean, InventoryLike, InventoryLike) = {
    if (canTake(o, quantity)) {
      (true, take(o, quantity), target.add(o, quantity))
    } else (false, this, target)
  }

  def getItemByName(itemName: String): Try[Storable] = {
    val acceptableItems = getContent.keySet.filter(i => i.displayName == itemName || i.displayName + "s" == itemName)

    if (acceptableItems.isEmpty)
      Failure(new IllegalArgumentException("there is no item by that name here..."))
    else if (acceptableItems.size > 1)
      Failure(new IllegalArgumentException("there are multiple items by that name here..."))
    else Success(acceptableItems.head)
  }
}

object InventoryLike {
  def extractItemNameAndQuantity(input: Array[String], defaultQuantity: Int = 1): (String, Int) = {
    val (quantity, itemNameParts) =
      if (input.head.forall(c => c.isDigit))
        (input.head.toInt, input drop 1)
      else if (input.head.equalsIgnoreCase("a"))
        (1, input drop 1)
      else
        (defaultQuantity, input)

    val itemName = (itemNameParts mkString " ").toLowerCase

    (itemName, quantity)
  }
}