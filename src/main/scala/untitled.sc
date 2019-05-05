import ch.epfl.lara.engine.game.actions.control.compiler.Lexer

val prog =
  """wait 1
    |when (trigger != null && trigger.entering == true && characters.me.currentRoom.id == "1st-floor") {
    |  if (time >= 6:00:00 && time < 18:00:00) {
    |    do "say Hello you!"
    |  } else {
    |    do "say What are you doing here?"
    |  }
    |}
    |
    |when (trigger != null && trigger.__name == "InventoryTradeRequest") {
    | do now "say Hmm... let's see what you're offering me... " + trigger.content.sentItems.0 + ", I see..."
    | if ("peanut" in trigger.content.sentItems) {
    |   do now "refuse"
    |   do now "say Please, no, no, no more peanuts...!"
    | } else {
    |   do now "accept"
    |   do now "say Thank you for this present!"
    | }
    | do "wait 2"
    |}
  """.stripMargin

println(prog)

val lexed = Lexer(prog)
println(lexed)
