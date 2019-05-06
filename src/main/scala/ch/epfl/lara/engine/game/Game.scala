package ch.epfl.lara.engine.game

import java.io.{OutputStream, PrintStream}

import ch.epfl.lara.engine.game.actions._
import ch.epfl.lara.engine.game.entities.{PPC, ProgrammedNPC, TraderNPC}
import ch.epfl.lara.engine.game.environment._
import ch.epfl.lara.engine.game.items.mutable.InventoryHolderItem
import ch.epfl.lara.engine.game.items.{ImmutableInventoryImpl, Pickable}

import scala.annotation.tailrec
import scala.io.StdIn

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

    GameState.get.scheduler.runRegular(5, 10)((_, _) => {
      cellar.add(peanut, 1)
    })

    GameState.get.scheduler.runRegular(0, 24 * 3600)((_, _) => {
      println(s"The sun rises...")
    })

    GameState.get.scheduler.runRegular(18 * 3600, 24 * 3600)((_, _) => {
      println(s"The sun starts to go down...")
    })

    GameState.get.scheduler.runRegular(21 * 3600, 24 * 3600)((_, _) => {
      println(s"The sky is now dark...")
    })

    // Create dummy NPCs
    /*val dummyNPC1 = PPC(
      new CharacterState(rooms.getRoom("store"), Center, "Shop Keeper", out = emptyStream),
        """
          |wait 1
        """.stripMargin,
      List(
        "anyone enters" ->
          """
            |if me has room == "store"
            |if time >= 6:00 && time < 18:00
            |say Hello you!
            |end
            |
            |if time >= 18:00 || time < 6:00
            |say The shop is closed... please leave!
            |end
            |end
          """.stripMargin,
        "anyone trades" ->
          """
            |accept
          """.stripMargin
      )
    )*/
    val dummyNPC0 = new PPC(
      new CharacterState(rooms.getRoom("1st-floor"), Center, "Somebody", out = emptyStream),
      """
        |do "wait 1"
        |
        |when (trigger != null && trigger.__name == "RoomMovement" && trigger.entering == true) {
        |  if (room.id == "1st-floor") {
        |   if (time >= 6:00:00 && time < 18:00:00) {
        |    do "say Hello you!"
        |   } else {
        |    do "say What are you doing here?"
        |   }
        |  } else if (lost != null && lost == true) {
        |   do "say I am lost... help me go back home please...!"
        |  }
        |}
        |
        |when (trigger != null && trigger.__name == "ReleasedControl") {
        | if (room.id == initRoom) {
        |   lost := false
        | } else {
        |   lost:= true
        |   do now "say What... Where am I?"
        | }
        |}
        |
        |when (trigger != null && trigger.__name == "TakenControl") {
        | initRoom := room.id
        |}
        |
        |when (trigger != null && trigger.__name == "InventoryTradeRequest") {
        | do now "say Hmm... let's see what you're offering me... " + trigger.sentItems.`0` + ", I see..."
        | if ("peanut" in trigger.sentItems) {
        |   do now "refuse"
        |   do now "say Please, no, no, no more peanuts...!"
        | } else {
        |   do now "accept"
        |   do now "say Thank you for this present!"
        | }
        | do "wait 2"
        |}
        |
        |/* Every hour */
        |if (time % 3600 == 0 && (lost == null || !lost)) {
        | do "go south"
        | do "go east"
        | if (room.inventory.content.peanut != null) {
        |   count := room.inventory.content.peanut
        |   do "say Argh, so many peanuts again... What a child!"
        |   do "take " + count + " peanuts"
        |   do "say " + count + " peanuts, really, can't he start doing something else?"
        |   do "open bin"
        |   do "drop " + count + " peanuts"
        |   do "close"
        | }
        | do "go west"
        | do "go north"
        |}
        |
        |if (lost != null && lost && time % 90 == 0) {
        |  sentenceNum := ((time % 270) / 90)
        |  if (sentenceNum == 0) do "say I want to see my familly back..."
        |  else if (sentenceNum == 1) do "say HELP! I AM LOST!"
        |  else if (sentenceNum == 2) do "say (whining) Where am I... Please..."
        |  else do "say Please? Can anybody help me?"
        |}
      """.stripMargin
      )

    val dummyNPC1 = new TraderNPC(rooms.getRoom("store"), Center, "Shop Keeper",
      Map(peanut -> 500, Pickable("nut") -> 500, Pickable("noiset") -> 100),
      Map(peanut -> 1, Pickable("nut") -> 2, Pickable("noiset") -> 5)
    )

    val dummyNPC2 = new PPC(
      new CharacterState(rooms.getRoom("1st-floor-dining-room"), Center, "Child", out = emptyStream),
        """
          |
          |if (moved == null) {
          | moved := 0
          |}
          |
          |if (characters.`1` != null) {
          | do "say Hello, who are you?"
          |}
          |do "open cellar"
          |do "take 1 peanut"
          |do "quit"
          |do "go east"
          |do "drop 1 peanut"
          |if (characters.`1` != null) {
          | do "say I love peanut butter!"
          |}
          |
          |moved := moved + 1
          |
          |when (trigger != null && trigger.__name == "TalkingMessage" && "hello" in trigger.content.toLowerCase && trigger.sentBy.name != "Child") {
          | do "say Hello! I've moved " + moved + " peanuts... That's a hard work you know?"
          |}
          |do "go west"
        """.stripMargin
    )

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
