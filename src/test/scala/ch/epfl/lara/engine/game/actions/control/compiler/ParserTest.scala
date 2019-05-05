package ch.epfl.lara.engine.game.actions.control.compiler

import ch.epfl.lara.engine.game.actions.control.ActionCompiler
import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.List

/**
  * @author Louis Vialar
  */
class ParserTest extends FlatSpec with Matchers {

  "The parser" should "parse easy conditions" in {

    ActionCompiler.compile(""""peanut" in player.inventory._names && event.type == "enters" + " " + room.name || "peanuts" in characters.`Shop Keeper`.inventory""") should be(
      Right(Tree.And(
        Tree.In(
          Tree.StringLiteral("peanut"),
          Tree.Identifier(List("player", "inventory", "_names"))
        ),
        Tree.Or(
          Tree.Eq(
            Tree.Identifier(List("event", "type")),
            Tree.Concat(Tree.StringLiteral("enters"), Tree.Concat(Tree.StringLiteral(" "), Tree.Identifier(List("room", "name"))))),
          Tree.In(
            StringLiteral("peanuts"),
            Tree.Identifier(List("characters", "Shop Keeper", "inventory"))
          )
      ))))

    ActionCompiler.compile("""trigger.type == "InventoryTradeRequest" && ! "peanut" in trigger.content.sentItem""") should be(Right(
      And(
        Eq(Identifier(List("trigger", "type")), StringLiteral("InventoryTradeRequest")),
        Not(In(StringLiteral("peanut"), Identifier(List("trigger", "content", "sentItem"))))
      )
    ))
  }
}