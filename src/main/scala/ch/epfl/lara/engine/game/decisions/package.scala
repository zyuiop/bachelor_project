package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.environment.Position
import ch.epfl.lara.engine.game.items.{Item, ItemRegistry}

/**
  * @author Louis Vialar
  */
package object decisions {
  trait Command

  case class MoveCommand(direction: Position) extends Command

  /**
    *
    * @param o
    * @param quantity 0 = all of this kind
    */
  case class ItemDropCommand(o: Item, quantity: Int) extends Command

  case class ItemInteractCommand(o: Option[Item]) extends Command

  case object ItemPickAllCommand extends Command

  case object ItemPickOneCommand extends Command

  /**
    *
    * @param o
    * @param quantity 0 = all of this kind
    */
  case class ItemPickCommand(o: Item, quantity: Int) extends Command

  case class ItemSeekCommand(direction: Option[Position]) extends Command

  case class TakeDoorCommand(direction: Option[Position]) extends Command

  case class InvalidCommand(error: String) extends Command

  case object SaveGameCommand extends Command
  case object QuitGameCommand extends Command

  /*
      Example commands:

      // When arriving in room, you get a brief description:

      A door at the east
      A cupboard at the north
      A table at the south
      A door at the west

      Available commands:
      - go|move <direction>

      - use|interact [object] (object not required if not ambiguous == if you already navigated to a direction where only an object can be used)

      - take|pass|use door [direction] (allow to use a door in a provided direction, or in the direction where you stand)

      - seek [direction] (allows you to discover items in the provided direction, or in the direction where you stand if not provided, or in the in-use inventory)

      - pick|take <object|all|one(default)> [amount] (allows you to pick previously discovered items)

      - drop <object> <amount> (allows you to drop items of your inventory in the current opened inventory (might be an area or an item like a cupboard))

       */

  object Command {
    private def parsePosition(args: Array[String])(ifSuccess: Position => Command): Command = {
      Position.parseEither(args.mkString("-")).fold(t => InvalidCommand(t.getMessage), ifSuccess)
    }

    private def parseItem(args: Array[String])(implicit items: ItemRegistry): (Option[Item], Int) = {
      if (args.head.forall(c => c.isDigit) || args.head.toLowerCase == "a" || args.head.toLowerCase == "all" || args.head.toLowerCase == "one") {
        val quantity =
          if (args.head.forall(c => c.isDigit)) args.head.toInt
          else if (args.head.toLowerCase == "all") 0
          else 1
        val item = items.getItem(args.tail.mkString(" "), quantity == 0 || quantity > 1)

        (item, quantity)
      } else {
        val quantity = args.last.toInt
        val item = items.getItem(args.dropRight(1).mkString(" "), quantity > 1)

        (item, quantity)
      }
    }

    def buildDecision(inputString: String)(implicit items: ItemRegistry): Command = {
      val keywords = inputString.split(" ")
      val action = keywords.head.toLowerCase
      val args = keywords.tail

      if (action == "go" || action == "move") {
        parsePosition(args)(MoveCommand)
      } else if (action == "pass" || action == "take" || (action == "use" && args.nonEmpty && args.head.toLowerCase == "door")) {
        val orientationArg = if (action == "use" || (args.nonEmpty && args.head.toLowerCase == "door")) args.tail else args

        if (orientationArg.nonEmpty) parsePosition(orientationArg)(o => TakeDoorCommand(Some(o)))
        else TakeDoorCommand(None)
      } else if (action == "use" || action == "interact") {
        val item = if (args.nonEmpty) items.getItem(args.mkString(" ")) else None
        ItemInteractCommand(item)
      } else if (action == "seek") {
        if (args.nonEmpty) parsePosition(args)(o => ItemSeekCommand(Some(o)))
        else ItemSeekCommand(None)
      } else if (action == "drop") {
        val (item, quantity) = parseItem(args)

        item match {
          case Some(i) => ItemDropCommand(i, quantity)
          case None => InvalidCommand("unknown item")
        }
      } else if (action == "pick" || action == "take") {
        if (args.isEmpty || (args.head.toLowerCase == "one" && args.tail.isEmpty))
          ItemPickOneCommand
        else if (args.head.toLowerCase == "all" && args.tail.isEmpty)
          ItemPickAllCommand
        else {
          val (item, quantity) = parseItem(args)

          item match {
            case Some(i) => ItemDropCommand(i, quantity)
            case None => InvalidCommand("unknown item")
          }
        }
      } else if (action == "save") {
        SaveGameCommand
      } else if (action == "quit") {
        QuitGameCommand
      } else {
        InvalidCommand("unknown action " + action)
      }
    }


  }


}
