package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.decisions.{Action, ActionBuilder, ActionParser}
import ch.epfl.lara.engine.game.items.Pickable

import scala.collection.mutable
import scala.util.Try

/**
  * Represents an inventory
  *
  * @author Louis Vialar
  */
class MutableInventoryImpl(initialContent: Map[Pickable, Int]) extends Inventory {

  override val actionParser: ActionParser = ActionParser(
    super.actionParser,
    new ActionBuilder[Action] {
      override def apply(input: Array[String]): Try[Action] = Try {
        val (item, quantity) = Inventory.parseItemNameAndQuantity(input drop 1, MutableInventoryImpl.this)

        new Action {
          override def execute(inState: LevelState)(implicit out: PrintStream): LevelState = {
            val (succ, _, right) = transferTo(inState.inventory, item, quantity)

            if (succ)
              inState.copy(inventory = right)
            else {
              out.println("There are not enough items to " + input.head.toLowerCase)
              inState
            }
          }
        }
      }

      override val triggeringKeywords: Set[String] = Set("take", "pick")
    },
    new ActionBuilder[Action] {
      override def apply(input: Array[String]): Try[Action] = Try {
        new Action {
          override def execute(inState: LevelState)(implicit out: PrintStream): LevelState = {
            try {
              val (item, quantity) = Inventory.parseItemNameAndQuantity(input drop 1, inState.inventory)
              val (succ, left, _) = inState.inventory.transferTo(MutableInventoryImpl.this, item, quantity)

              if (succ)
                inState.copy(inventory = left)
              else {
                out.println("There are not enough items to " + input.head.toLowerCase)
                inState
              }
            } catch {
              case e: IllegalArgumentException =>
                out.println(e.getMessage)
                inState
            }
          }
        }
      }

      override val triggeringKeywords: Set[String] = Set("drop")
    }
  )

  private val content = mutable.Map(initialContent.toList:_*)

  def take(o: Pickable, quantity: Int): MutableInventoryImpl = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (content.getOrElse(o, 0) < quantity)
      throw new IllegalStateException("insufficient quantity")

    content
      .transform((pickable, amount) => if (pickable == o) amount - quantity else amount)
        .retain((_, amount) => amount > 0)

    this
  }

  def add(o: Pickable, quantity: Int): MutableInventoryImpl = {
    if (quantity < 0)
      throw new IllegalArgumentException("quantity < 0")

    if (content.contains(o))
      content
        .transform((pickable, amount) => if (pickable == o) amount + quantity else amount)
    else content put (o, quantity)

    this
  }

  def canTake(o: Pickable, quantity: Int): Boolean =
    quantity > 0 && content.getOrElse(o, 0) < quantity

  override def getContent: Map[Pickable, Int] = content.toMap
}

