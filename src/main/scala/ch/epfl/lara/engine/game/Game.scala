package ch.epfl.lara.engine.game

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.entities.Interactable
import ch.epfl.lara.engine.game.environment._
import ch.epfl.lara.engine.game.items.{Item, ItemRegistry, Pickable}
import ch.epfl.lara.engine.game.items.mutable.InventoryHolderItem

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * @author Louis Vialar
  */
object Game {

  private var running: Boolean = true


  private implicit val printStream: PrintStream = Console.out

  def main(args: Array[String]): Unit = {

    val peanut = Pickable("peanut")

    val rooms = RoomRegistry(Seq(
      new Room("street", "42nd Street", "The sun is rising. The crowd is moving between buildings.", Map(), Map()),
      new Room("store", "Convenience Store", "Day to day items are around the shelves", Map(peanut -> 5), Map()),
      new Room("1st-floor", "1st Floor", "Boxes", Map(), Map()),
      new Room("1st-floor-bathroom", "Bathroom", "It's quite clean", Map(), Map()),
      new Room("1st-floor-dining-room", "Dining Room", "A table, 4 chairs", Map(),
        Map("cellar" -> Map(South -> new InventoryHolderItem("cellar",
          new ImmutableInventoryImpl(Map(peanut -> 3)))))),
      new Room("1st-floor-kitchen", "Kitchen", "Also a small table I guess", Map(), Map()),
      new Room("1st-floor-bedroom", "Bedroom", "The bed is not properly cleaned", Map(), Map())
    ),
      Seq(
        Door("street", "store", North, South, DoorType.Door),
        Door("store", "1st-floor", North, North, DoorType.Stairs),
        Door("1st-floor", "1st-floor-bathroom", East, West, DoorType.Door),
        Door("1st-floor", "1st-floor-dining-room", South, North, DoorType.Door),
        Door("1st-floor-kitchen", "1st-floor-dining-room", West, East, DoorType.Door),
        Door("1st-floor", "1st-floor-bedroom", West, East, DoorType.Door),
        Door("1st-floor-dining-room", "1st-floor-bedroom", West, East, DoorType.Door)
      ))

    val items = ItemRegistry(Map())

    val map = LevelMap(items, rooms)

    new GameState(map)

    val state = PlayerState(new ImmutableInventoryImpl(Map()), rooms.getRoom("street"), Center, None, Map(), map, List(ActionParser.DefaultParser))(Console.out)

    println(state.currentRoom.describe(state.map))

    loop(state)
  }

  private val systemActionsParser = ActionParser(
    ActionSaveGame // TODO: add quit, ...
  )

  def saveGame(state: PlayerState) = ???

  def loadGame(): PlayerState = ???

  def quitGame(): Unit = {
    running = false
  }

  @tailrec
  def loop(state: PlayerState): Unit = {
    if (!running) {
      printStream.println("Good bye!")
      return
    }

    val nextStep = StdIn.readLine("> ").split(" ")
    val parser = state.currentParser.union(systemActionsParser)
    val action = parser(nextStep)

    if (action.isSuccess) {
      loop(action.get.execute(state))
    } else {
      println(action.failed.get.getMessage)
      loop(state)
    }
  }
}
