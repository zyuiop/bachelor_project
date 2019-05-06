package ch.epfl.lara.engine.game

import java.io.{File, OutputStream, PrintStream}

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.entities.{PPC, ProgrammedNPC, TraderNPC}
import ch.epfl.lara.engine.game.environment._
import ch.epfl.lara.engine.game.items.mutable.InventoryHolderItem
import ch.epfl.lara.engine.game.items.{ImmutableInventoryImpl, Pickable}

import scala.annotation.tailrec
import scala.io.{Source, StdIn}

/**
  * @author Louis Vialar
  */
object Game {

  private var running: Boolean = true


  private implicit val printStream: PrintStream = Console.out

  val emptyStream = new PrintStream((b: Int) => ())

  def main(args: Array[String]): Unit = {

    val peanut = Pickable("peanut")

    val cellar = new InventoryHolderItem("cellar", Map(peanut -> 3))
    val bin = new InventoryHolderItem("bin", Map())

    val rooms = RoomRegistry(Seq(
      new Room("street", "42nd Street", "The sun is rising. The crowd is moving between buildings.", Map(), Map()),
      new Room("store", "Convenience Store", "Day to day items are around the shelves", Map(peanut -> 5), Map()),
      new Room("1st-floor", "1st Floor", "Boxes", Map(), Map()),
      new Room("1st-floor-bathroom", "Bathroom", "It's quite clean", Map(), Map()),
      new Room("1st-floor-dining-room", "Dining Room", "A table, 4 chairs", Map(),
        Map("cellar" -> Map(South -> cellar))),
      new Room("1st-floor-kitchen", "Kitchen", "Also a small table I guess", Map(),
        Map("bin" -> Map(East -> bin))
      ),
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

    val map = LevelMap(rooms)

    new GameState(map, 6 * 3600) // 6 AM

    GameState.get.scheduler.runRegular(5, 10)(_ => {
      cellar.add(peanut, 1)
    })

    GameState.get.scheduler.runRegular(0, 24 * 3600)(_ => {
      println(s"The sun rises...")
    })

    GameState.get.scheduler.runRegular(18 * 3600, 24 * 3600)(_ => {
      println(s"The sun starts to go down...")
    })

    GameState.get.scheduler.runRegular(21 * 3600, 24 * 3600)(_ => {
      println(s"The sky is now dark...")
    })

    val dummyNPC0 = CharacterParser(Source.fromFile(new File("data/level-1/shopkeeper.txt")).mkString)(rooms.getRoom)
    val dummyNPC1 = CharacterParser(Source.fromFile(new File("data/level-1/somebody.txt")).mkString)(rooms.getRoom)
    val dummyNPC2 = CharacterParser(Source.fromFile(new File("data/level-1/child.txt")).mkString)(rooms.getRoom)

    dummyNPC0.spawn()
    dummyNPC1.spawn()
    dummyNPC2.spawn()

    // Create Player State
    val state = new PlayerState(rooms.getRoom("street"), Map(GameState.Currency -> 1000, peanut -> 50, Pickable("nut") -> 10))
    state.spawn()

    println(state.currentRoom.describe())

    loop(state)
  }

  private val systemActionsParser = ActionParser(
    ActionSaveGame // TODO: add quit, ...
  )

  def saveGame(state: CharacterState) = ???

  def loadGame(): CharacterState = ???

  def quitGame(): Unit = {
    running = false
  }

  val parser: ActionParser = ActionParser.DefaultParser union systemActionsParser

  @tailrec
  def loop(state: CharacterState): Unit = {
    if (!running) {
      printStream.println("Good bye!")
      return
    }

    // Check for level transitions
    GameState.level

    // Run action
    val nextStep = StdIn.readLine("> ").split(" ")
    val action = parser(nextStep)

    if (action.isSuccess) {
      val time = action.get.execute(state)
      GameState.get.scheduler.addTime(time)
      loop(state)
    } else {
      println(action.failed.get.getMessage)
      loop(state)
    }
  }
}
