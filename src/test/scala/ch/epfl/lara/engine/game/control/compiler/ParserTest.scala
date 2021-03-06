package ch.epfl.lara.engine.game.control.compiler

import ch.epfl.lara.engine.game.control.ActionCompiler
import ch.epfl.lara.engine.game.control.compiler.Tree._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.List

/**
  * @author Louis Vialar
  */
class ParserTest extends FlatSpec with Matchers {
  "The parser" should "parse easy conditions" in {

    ActionCompiler.compileValue("test program", """("peanut" in player.inventory._names && event.type == "enters" + " " + room.name || "peanuts" in characters.`Shop Keeper`.inventory)""") should be(
      (Tree.And(
        Tree.In(
          Tree.StringLiteral("peanut"),
          Tree.Identifier(List("player", "inventory", "_names"))
        ),
        Tree.Or(
          Tree.Eq(
            Tree.Identifier(List("event", "type")),
            Tree.Sum(Tree.StringLiteral("enters"), Tree.Sum(Tree.StringLiteral(" "), Tree.Identifier(List("room", "name"))))),
          Tree.In(
            StringLiteral("peanuts"),
            Tree.Identifier(List("characters", "Shop Keeper", "inventory"))
          )
        ))))

    ActionCompiler.compileValue("test program", """(trigger.type == "InventoryTradeRequest" && ! "peanut" in trigger.content.sentItem)""") should be(
      And(
        Eq(Identifier(List("trigger", "type")), StringLiteral("InventoryTradeRequest")),
        Not(In(StringLiteral("peanut"), Identifier(List("trigger", "content", "sentItem"))))
      )
    )
  }

  it should "parse binary conditions" in {
    ActionCompiler.compileValue("test program", """(true || false && false || true)""") should be(
      Or(BooleanLiteral(true),
        And(BooleanLiteral(false),
          Or(BooleanLiteral(false), BooleanLiteral(true)))
      ))

    ActionCompiler.compileValue("test program", """((true || false) && (false || true))""") should be(
      And(
        Or(BooleanLiteral(true), BooleanLiteral(false)),
        Or(BooleanLiteral(false), BooleanLiteral(true)))
    )

  }

  it should "parse a basic operation" in {
    ActionCompiler.compileValue("test program", """(1 + 2 + 3 - id)""") should be(
      Sum(IntLiteral(1), Sum(IntLiteral(2), Difference(IntLiteral(3), Identifier(List("id")))))
    )
  }

  it should "parse a basic chain of expressions" in {
    ActionCompiler.compileProgram("test program", 
      """
        |do "something"
        |do now "something else"
        |do "something" + "with" + concat
      """.stripMargin)._1 should be(
      Sequence(List(
        Do(StringLiteral("something"), false, false),
        Do(StringLiteral("something else"), true, false),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false, false)
      ))
    )
  }
  it should "parse a basic block of expressions" in {
    ActionCompiler.compileProgram("test program", 
      """
        |{
        |do "something"
        |do now "something else"
        |do blocking "something" + "with" + concat
        |}
      """.stripMargin)._1 should be(
      Sequence(List(
        Do(StringLiteral("something"), false, false),
        Do(StringLiteral("something else"), true, false),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false, true)
      ))
    )
  }
  it should "parse a basic block of expressions in the middle of expressions" in {
    ActionCompiler.compileProgram("test program", 
      """
        |do "say plop"
        |{
        |do "something"
        |do now "something else"
        |}
        |do "something" + "with" + concat
        |
      """.stripMargin)._1 should be(
      Sequence(List(
        Do(StringLiteral("say plop"), false, false),
        Sequence(List(
          Do(StringLiteral("something"), false, false),
          Do(StringLiteral("something else"), true, false))),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false, false)
      ))
    )
  }


  it should "parse a basic set of conditions" in {
    ActionCompiler.compileProgram("test program", 
      """
        |do "say plop"
        |if (id == otherId && true) {
        |do "something"
        |} else {
        | if (id == false) {
        |   do now "something else"
        |} }
        |do "something" + "with" + concat
        |when (cond == true && false) {
        |do "say cheese"
        |do "thanks"
        |}
        |
      """.stripMargin) should be((
      Sequence(List(
        Do(StringLiteral("say plop"), false, false),
        Ite(And(Eq(Identifier(List("id")), Identifier(List("otherId"))), BooleanLiteral(true)),
          Do(StringLiteral("something"), false, false),
          Ite(Eq(Identifier(List("id")), BooleanLiteral(false)),
            Do(StringLiteral("something else"), true, false),
            EmptyExpr())
        ),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false, false))),
      List(
        When(
          And(Eq(Identifier(List("cond")), BooleanLiteral(true)), BooleanLiteral(false)),
          Sequence(List(
            Do(StringLiteral("say cheese"), false, false),
            Do(StringLiteral("thanks"), false, false)
          ))
        )
      ), List()
    ))
  }


  it should "parse a basic set of conditions with no brackets" in {
    ActionCompiler.compileProgram("test program", 
      """
        |do "say plop"
        |if (id == otherId && true)
        |   do "something"
        |else if (id == false)
        |   do now "something else"
        |
        |do "something" + "with" + concat
        |when (cond == true && false)(-10) {
        |do "say cheese"
        |do "thanks"
        |}
        |when (cond == false && true)(10) {
        |do "say helooo"
        |do "thanks"
        |}
        |
      """.stripMargin) should be((
      Sequence(List(
        Do(StringLiteral("say plop"), false, false),
        Ite(And(Eq(Identifier(List("id")), Identifier(List("otherId"))), BooleanLiteral(true)),
          Do(StringLiteral("something"), false, false),
          Ite(Eq(Identifier(List("id")), BooleanLiteral(false)),
            Do(StringLiteral("something else"), true, false),
            EmptyExpr())
        ),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false, false),
      )), List(When(
      And(Eq(Identifier(List("cond")), BooleanLiteral(true)), BooleanLiteral(false)),
      Sequence(List(
        Do(StringLiteral("say cheese"), false, false),
        Do(StringLiteral("thanks"), false, false)
      )),
      -10
    ),
      When(
        And(Eq(Identifier(List("cond")), BooleanLiteral(false)), BooleanLiteral(true)),
        Sequence(List(
          Do(StringLiteral("say helooo"), false, false),
          Do(StringLiteral("thanks"), false, false)
        )),
        10
      )
    ), List()
    ))
  }

  it should "parse NPC program correctly" in {
    ActionCompiler.compileProgram("test program", 
      """if (time % 3600 == 0 && (lost == null || !lost)) {
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
        |}""".stripMargin)._1 should be(

      Ite(And(
        Eq(Module(Identifier(List("time")), IntLiteral(3600)), IntLiteral(0)),
        Or(Eq(Identifier(List("lost")), NullLiteral()), Not(Identifier(List("lost"))))
      ), Sequence(
        List(
          Do(StringLiteral("go south"), false, false),
          Do(StringLiteral("go east"), false, false),
          Ite(Neq(Identifier(List("room", "inventory", "content", "peanut")), NullLiteral()),
            Sequence(List(
              Set(Identifier(List("count")), Identifier(List("room", "inventory", "content", "peanut"))),
              Do(StringLiteral("say Argh, so many peanuts again... What a child!"), false, false),
              Do(Sum(StringLiteral("take "), Sum(Identifier(List("count")), StringLiteral(" peanuts"))), false, false),
              Do(Sum(StringLiteral("say "), Sum(Identifier(List("count")), StringLiteral(" peanuts, really, can't he start doing something else?"))), false, false),
              Do(StringLiteral("open bin"), false, false),
              Do(Sum(StringLiteral("drop "), Sum(Identifier(List("count")), StringLiteral(" peanuts"))), false, false),
              Do(StringLiteral("close"), false, false)


            ))
            , EmptyExpr()),

          Do(StringLiteral("go west"), false, false),
          Do(StringLiteral("go north"), false, false),
        )
      )
        ,
        EmptyExpr()
      )

    )
  }
}