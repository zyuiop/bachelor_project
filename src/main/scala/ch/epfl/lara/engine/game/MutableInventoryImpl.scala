package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.{Action, ActionBuilder, ActionParser}
import ch.epfl.lara.engine.game.items.Pickable

import scala.collection.mutable
import scala.util.Try

/**
  * Represents an inventory
  *
  * @author Louis Vialar
  */
class MutableInventoryImpl(initialContent: Map[Pickable, Int]) extends Inventory {
  /* TODO: parsing while knowing is fun,
      but actually we should not parse commands knowing the
      context in which they will be executed,
      because that's meaningless
   */
  @deprecated
  private lazy val completeActionParser: ActionParser = ActionParser(
    super.actionParser,
    new ActionBuilder[Action] {
      override def apply(input: Array[String]): Try[Action] = Try {
        val (item, quantity) = Inventory.parseItemNameAndQuantity(input drop 1, MutableInventoryImpl.this)

        (inState: PlayerState, out: PrintStream) => {
          val (succ, _, right) = transferTo(inState.inventory, item, quantity)

          if (succ) {
            out.println(s"You took $quantity * ${item.displayName} into your inventory.")
            (inState.copy(inventory = right)(out), 5)
          } else {
            out.println("There are not enough items to " + input.head.toLowerCase)
            (inState, 3)
          }
        }
      }

      override val triggeringKeywords: Set[String] = Set("take", "pick")
    },
    new ActionBuilder[Action] {
      override def apply(input: Array[String]): Try[Action] = Try {
        (inState: PlayerState, out: PrintStream) => {
          try {
            val (item, quantity) = Inventory.parseItemNameAndQuantity(input drop 1, inState.inventory)
            val (succ, left, _) = inState.inventory.transferTo(MutableInventoryImpl.this, item, quantity)

            if (succ) {
              out.println(s"You dropped $quantity * ${item.displayName} from your inventory.")
              (inState.copy(inventory = left)(out), 5)
            } else {
              out.println("There are not enough items to " + input.head.toLowerCase)
              (inState, 3)
            }
          } catch {
            case e: IllegalArgumentException =>
              out.println(e.getMessage)
              (inState, 0)
          }
        }
      }

      override val triggeringKeywords: Set[String] = Set("drop")
    }
  )

  override def actionParser = completeActionParser

  private val content = mutable.Map(initialContent.toList: _*)

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
    else content put(o, quantity)

    this
  }

  def canTake(o: Pickable, quantity: Int): Boolean =
    quantity > 0 && content.getOrElse(o, 0) >= quantity

  override def getContent: Map[Pickable, Int] = content.toMap
}

