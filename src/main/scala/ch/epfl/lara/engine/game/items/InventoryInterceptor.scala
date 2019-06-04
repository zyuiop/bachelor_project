package ch.epfl.lara.engine.game.items

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.ActionInterceptor

import scala.util.Try
;

/**
  * An interceptor that handles common inventory commands (list, drop, take)
  * @author Louis Vialar
  */
trait InventoryInterceptor extends ActionInterceptor with InventoryLike {
  handle("list", "search", "probe") { (state, _) =>
    printContent(state.ps)
    5
  }

  handle("drop") { (state, args) =>
    transferItem(args, state.inventory, this, "dropped", "from", state.ps)
  }

  handle("take", "pick") { (state, args) =>
    transferItem(args, this, state.inventory, "took", "into", state.ps)
  }

  private def transferItem(input: Array[String], source: InventoryLike, target: InventoryLike, verb: String, dirWord: String, ps: PrintStream): Int = {
    val (item, quantity) = InventoryLike.extractItemNameAndQuantity(input)

    source.getItemByName(item).flatMap(item => {
      Try {
        val (succ, _, _) = source.transferTo(target, item, quantity)

        if (succ) {
          ps.println(s"You $verb $quantity * ${item.displayName} $dirWord your inventory.")
          5
        } else {
          ps.println("Not enough items!")
          3
        }
      }
    }).recover({
      case ex: IllegalArgumentException =>
        ps.println(ex.getMessage)
        0
    }).get
  }
}
