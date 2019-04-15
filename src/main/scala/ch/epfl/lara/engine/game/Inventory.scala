package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder, ActionParser}
import ch.epfl.lara.engine.game.items.Pickable

import scala.util.Try

/**
  * @author Louis Vialar
  */
trait Inventory {
  private lazy val basicActionParser = ActionParser(
    new ActionBuilder[Action] {
      override def apply(input: Array[String]): Try[Action] = Try {
        (inState, out) => {
          printContent(out)
          inState
        }
      }

      override val triggeringKeywords: Set[String] = Set("list", "search", "probe")
    }
  )

  def actionParser: ActionParser = basicActionParser

  def printContent(implicit printStream: PrintStream): Unit = {
    for ((item, quantity) <- getContent)
      printStream.println(s"\t$quantity\t*\t${item.displayName}")
  }

  def getContent: Map[Pickable, Int]

  /**
    * Take an item in this inventory, returning the updated inventory
    * @param o the item to take
    * @param quantity the amount of this item to take
    * @return the updated inventory
    */
  def take(o: Pickable, quantity: Int): Inventory

  /**
    * Add an item to this inventory, returning the updated inventory
    * @param o the item to add
    * @param quantity the amount of this item to add
    * @return the updated inventory
    */
  def add(o: Pickable, quantity: Int): Inventory

  /**
    * Check if an item can be taken from this inventory
    * @param o the item to take
    * @param quantity the quantity we wish to take
    * @return true if we can take this quantity of this item
    */
  def canTake(o: Pickable, quantity: Int): Boolean

  /**
    * Transfer objects from the current inventory to an other one. This operation returns a triple, containing the
    * success status of the operation, the updated source inventory, and the updated target inventory.
    *
    * @param target   the target inventory
    * @param o        the object to transfer
    * @param quantity the quantity of objects to transfer
    * @return a triple (success, source, target)
    */
  def transferTo(target: Inventory, o: Pickable, quantity: Int): (Boolean, Inventory, Inventory) = {
    if (canTake(o, quantity)) {
      (true, take(o, quantity), target.add(o, quantity))
    } else (false, this, target)
  }
}

object Inventory {
  val empty: Inventory = new Inventory {
    override def getContent: Map[Pickable, Int] = Map.empty

    override def take(o: Pickable, quantity: Int): Inventory = throw new IllegalArgumentException("not enough items in inventory")

    override def add(o: Pickable, quantity: Int): Inventory = throw new IllegalStateException("cannot add in empty inventory")

    override def canTake(o: Pickable, quantity: Int): Boolean = false
  }

  def parseItemNameAndQuantity(input: Array[String], inv: Inventory, defaultQuantity: Int = 1): (Pickable, Int) = {
    val (quantity, itemNameParts) =
      if (input.head.forall(c => c.isDigit))
        (input.head.toInt, input drop 1)
      else if (input.head.equalsIgnoreCase("a"))
        (1, input drop 1)
      else
        (defaultQuantity, input)

    val itemName = (itemNameParts mkString " ").toLowerCase
    val acceptableItems = inv.getContent.keySet.filter(i => i.displayName == itemName || i.displayName + "s" == itemName)

    if (acceptableItems.isEmpty)
      throw new IllegalArgumentException("there is no item by that name here...")
    else if (acceptableItems.size > 1)
      throw new IllegalArgumentException("there are multiple items by that name here...")
    else (acceptableItems.head, quantity)
  }
}