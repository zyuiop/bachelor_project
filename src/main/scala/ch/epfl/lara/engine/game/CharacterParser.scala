package ch.epfl.lara.engine.game

import java.io.{PrintStream, Reader}

import ch.epfl.lara.engine.game.entities.{NPC, PPC, ProgrammedNPC, TraderNPC}
import ch.epfl.lara.engine.game.environment.{Position, Room}
import ch.epfl.lara.engine.game.items.Pickable

import scala.util.parsing.combinator.RegexParsers

/**
  * @author Louis Vialar
  */
object CharacterParser extends RegexParsers {
  def simpleIdentifier: Parser[String] = "[a-zA-Z_][a-zA-Z0-9._]*".r

  def stringLiteral: Parser[String] = """"[^"]*"""".r ^^ { str => str drop 1 dropRight 1 }


  def keyValue = simpleIdentifier ~ "=" ~! stringLiteral ^^ { case l ~ eq ~ r => (l, r) }

  def properties = keyValue.+ ^^ { l => l.toMap }

  def program = ".+".r.* ^^ { l => l.mkString("\n") }

  def file = "[params]" ~ properties ~ ("[program]" ~ program).? ^^ {
    case _ ~ props ~ Some(_ ~ prog) => (props, Some(prog))
    case _ ~ props ~ None => (props, None)
  }

  def apply(content: String)(implicit rooms: String => Room): CharacterState with NPC = {
    parseResult(parse(file, content))
  }

  def apply(content: Reader)(implicit rooms: String => Room): CharacterState with NPC = {
    parseResult(parse(file, content))
  }

  private def parseResult(res: ParseResult[(Map[String, String], Option[String])])(implicit rooms: String => Room): CharacterState with NPC = res match {
    case NoSuccess(msg, next) => throw new Exception(msg)
    case Success(result, _) =>
      val props = result._1


      def prefixed(prefix: String): Map[String, String] = {
        val len = prefix.length + 1
        props.filter(_._1.startsWith(prefix + ".")).map(pair => pair._1.drop(len) -> pair._2)
      }


      val room = rooms(props("room"))
      val position = Position.parse(props.getOrElse("position", "Center"))
      val name = props("name")

      val kind = props.getOrElse("type", "npc").toLowerCase()

      val inv = prefixed("inv").map(pair => Pickable(pair._1) -> pair._2.toInt)

      if (kind == "trader") {
        val prices = prefixed("price").map(pair => Pickable(pair._1) -> pair._2.toInt)

        new TraderNPC(room, position, name, inv, prices)
      } else {
        val attr = prefixed("attr")
        val cstate = new CharacterState(room, position, name, inv, attr, new PrintStream(_ => ()))

        if (kind == "ppc") new PPC(cstate, result._2.getOrElse(""))
        else new ProgrammedNPC(cstate, result._2.getOrElse(""))
      }
  }

}
