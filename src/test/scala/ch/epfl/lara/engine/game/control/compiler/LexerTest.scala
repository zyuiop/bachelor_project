package ch.epfl.lara.engine.game.control.compiler

import ch.epfl.lara.engine.game.control.ActionCompiler
import ch.epfl.lara.engine.game.control.compiler.Tokens._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.List

/**
  * @author Louis Vialar
  */
class LexerTest extends FlatSpec with Matchers {

  "The lexer" should "parse easy programs" in {
    val lst = List(
      StringLiteral("peanut"), In(), Identifier("player"), Dot(), Identifier("inventory"), And(), Identifier("event"), Dot(), Identifier("type"), Eq(), StringLiteral("enters")
    )

    val p = Lexer(""""peanut" in player.inventory && event.type == "enters"""").right

    p.get should be(lst)

    Lexer("""trigger.type == "InventoryTradeRequest" && ! "peanut" in trigger.content.sentItem""") should be(Right(List(
      Identifier("trigger"), Dot(), Identifier("type"), Eq(), StringLiteral("InventoryTradeRequest"), And(),
      Not(), StringLiteral("peanut"), In(), Identifier("trigger"), Dot(), Identifier("content"), Dot(), Identifier("sentItem")
    )))

    Lexer("""trigger != null""") should be(Right(List(
      Identifier("trigger"), Neq(), Null()
    )))
  }

  it should "parse times" in {

    Lexer("15:00:00").right.get should be(List(IntLiteral(54000)))
    Lexer("time >= 6:00:00") should be(Right(List(
      Identifier("time"),
      Gte(),
      IntLiteral(21600)
    )))
    Lexer("time >= 6:00:00 && time < 18:00:00") should be(Right(List(
      Identifier("time"),
      Gte(),
      IntLiteral(21600),
      And(),
      Identifier("time"),
      Lt(),
      IntLiteral(64800)
    )))
  }

  it should "parse basic boolean expressions" in {

    Lexer("""(true || false) && (false || true)""") should be(Right(List(
      LPar(), BooleanLiteral(true), Or(), BooleanLiteral(false), RPar(), And(), LPar(),
      BooleanLiteral(false), Or(), BooleanLiteral(true), RPar()
    )))

    Lexer("""true || false && false || true""") should be(Right(List(
      BooleanLiteral(true), Or(), BooleanLiteral(false), And(),
          BooleanLiteral(false), Or(), BooleanLiteral(true)
    )))
  }

  it should "parse int literals" in {

    Lexer("""10""") should be(Right(List(
      IntLiteral(10)
    )))

    Lexer("""-10""") should be(Right(List(
      IntLiteral(-10)
    )))

  }
}