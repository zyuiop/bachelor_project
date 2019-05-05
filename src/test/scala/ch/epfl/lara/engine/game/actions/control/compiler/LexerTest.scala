package ch.epfl.lara.engine.game.actions.control.compiler

import ch.epfl.lara.engine.game.actions.control.ActionCompiler
import ch.epfl.lara.engine.game.actions.control.compiler.Tokens._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.List

/**
  * @author Louis Vialar
  */
class LexerTest extends FlatSpec with Matchers {

  "The lexer" should "parse easy programs" in {
    val lst = List(
      StringLiteral("peanut"), In, Identifier("player"), Dot, Identifier("inventory"), And, Identifier("event"), Dot, Identifier("type"), Eq, StringLiteral("enters")
    )

    val p = Lexer(""""peanut" in player.inventory && event.type == "enters"""").right

    p.get should be(lst)

    Lexer("""trigger.type == "InventoryTradeRequest" && ! "peanut" in trigger.content.sentItem""") should be(Right(List(
      Identifier("trigger"), Dot, Identifier("type"), Eq, StringLiteral("InventoryTradeRequest"), And,
      Not, StringLiteral("peanut"), In, Identifier("trigger"), Dot, Identifier("content"), Dot, Identifier("sentItem")
    )))
  }
  "The lexer" should "parse times" in {

    Lexer("15:00:00").right.get should be(List(IntLiteral(54000)))
    Lexer("time >= 6:00:00") should be(Right(List(
      Identifier("time"),
      Hte,
      IntLiteral(21600)
    )))
    Lexer("time >= 6:00:00 && time < 18:00:00") should be(Right(List(
      Identifier("time"),
      Hte,
      IntLiteral(21600),
      And,
      Identifier("time"),
      Lt,
      IntLiteral(64800)
    )))
  }
}