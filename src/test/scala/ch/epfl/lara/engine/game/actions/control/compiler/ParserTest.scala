package ch.epfl.lara.engine.game.actions.control.compiler

import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.List

/**
  * @author Louis Vialar
  */
class ParserTest extends FlatSpec with Matchers {

  def compileCondition(condition: String): Either[CompileError, LogicalExpression] = {
    for {
      tokens <- Lexer(condition).right
      tree <- Parser.parseLogicalExpression(tokens).right
    } yield tree
  }

  def compile(prog: String): Either[CompileError, Expression] = {
    for {
      tokens <- Lexer(prog).right
      tree <- Parser(tokens).right
    } yield tree
  }


  "The parser" should "parse easy conditions" in {

    compileCondition(""""peanut" in player.inventory._names && event.type == "enters" + " " + room.name || "peanuts" in characters.`Shop Keeper`.inventory""") should be(
      Right(Tree.And(
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

    compileCondition("""trigger.type == "InventoryTradeRequest" && ! "peanut" in trigger.content.sentItem""") should be(Right(
      And(
        Eq(Identifier(List("trigger", "type")), StringLiteral("InventoryTradeRequest")),
        Not(In(StringLiteral("peanut"), Identifier(List("trigger", "content", "sentItem"))))
      )
    ))
  }

  it should "parse binary conditions" in {
    compileCondition("""true || false && false || true""") should be(Right(
      Or(BooleanLiteral(true),
        And(BooleanLiteral(false),
          Or(BooleanLiteral(false), BooleanLiteral(true))))
    ))

    compileCondition("""(true || false) && (false || true)""") should be(Right(
      And(
        Or(BooleanLiteral(true), BooleanLiteral(false)),
        Or(BooleanLiteral(false), BooleanLiteral(true)))
    ))

  }

  it should "parse a basic chain of expressions" in {
    compile(
      """
        |do "something"
        |do now "something else"
        |do "something" + "with" + concat
      """.stripMargin) should be(Right(
      Sequence(List(
        Do(StringLiteral("something"), false),
        Do(StringLiteral("something else"), true),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false)
      ))
    ))
  }
  it should "parse a basic block of expressions" in {
    compile(
      """
        |{
        |do "something"
        |do now "something else"
        |do "something" + "with" + concat
        |}
      """.stripMargin) should be(Right(
      Sequence(List(
        Do(StringLiteral("something"), false),
        Do(StringLiteral("something else"), true),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false)
      ))
    ))
  }
  it should "parse a basic block of expressions in the middle of expressions" in {
    compile(
      """
        |do "say plop"
        |{
        |do "something"
        |do now "something else"
        |}
        |do "something" + "with" + concat
        |
      """.stripMargin) should be(Right(
      Sequence(List(
        Do(StringLiteral("say plop"), false),
        Sequence(List(
          Do(StringLiteral("something"), false),
          Do(StringLiteral("something else"), true))),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false)
      ))
    ))
  }


  it should "parse a basic set of conditions" in {
    compile(
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
      """.stripMargin) should be(Right(
      Sequence(List(
        Do(StringLiteral("say plop"), false),
        Ite(And(Eq(Identifier(List("id")), Identifier(List("otherId"))), BooleanLiteral(true)),
          Do(StringLiteral("something"), false),
          Ite(Eq(Identifier(List("id")), BooleanLiteral(false)),
            Do(StringLiteral("something else"), true),
            EmptyExpr())
        ),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false),
        When(
          And(Eq(Identifier(List("cond")), BooleanLiteral(true)), BooleanLiteral(false)),
          Sequence(List(
            Do(StringLiteral("say cheese"), false),
            Do(StringLiteral("thanks"), false)
          ))
        )
      )
      )))
  }



  it should "parse a basic set of conditions with no brackets" in {
    compile(
      """
        |do "say plop"
        |if (id == otherId && true)
        |   do "something"
        |else if (id == false)
        |   do now "something else"
        |
        |do "something" + "with" + concat
        |when (cond == true && false) {
        |do "say cheese"
        |do "thanks"
        |}
        |when (cond == false && true) {
        |do "say helooo"
        |do "thanks"
        |}
        |
      """.stripMargin) should be(Right(
      Sequence(List(
        Do(StringLiteral("say plop"), false),
        Ite(And(Eq(Identifier(List("id")), Identifier(List("otherId"))), BooleanLiteral(true)),
          Do(StringLiteral("something"), false),
          Ite(Eq(Identifier(List("id")), BooleanLiteral(false)),
            Do(StringLiteral("something else"), true),
            EmptyExpr())
        ),
        Do(Sum(StringLiteral("something"), Sum(StringLiteral("with"), Identifier(List("concat")))), false),
        When(
          And(Eq(Identifier(List("cond")), BooleanLiteral(true)), BooleanLiteral(false)),
          Sequence(List(
            Do(StringLiteral("say cheese"), false),
            Do(StringLiteral("thanks"), false)
          ))
        ),
        When(
          And(Eq(Identifier(List("cond")), BooleanLiteral(false)), BooleanLiteral(true)),
          Sequence(List(
            Do(StringLiteral("say helooo"), false),
            Do(StringLiteral("thanks"), false)
          ))
        )
      )
      )))
  }
}