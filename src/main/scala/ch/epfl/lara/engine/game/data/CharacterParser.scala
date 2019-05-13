package ch.epfl.lara.engine.game.data

import java.io.{PrintStream, Reader}

import ch.epfl.lara.engine.game.entities.{CharacterState, NPC, PPC, ProgrammedNPC, TraderNPC}
import ch.epfl.lara.engine.game.environment.{Position, Room}
import ch.epfl.lara.engine.game.items.Pickable

/**
  * @author Louis Vialar
  */
object CharacterParser extends BaseParser {

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

      val room = rooms(props("room"))
      val position = Position.parse(props.getOrElse("position", "Center"))
      val name = props("name")

      val kind = props.getOrElse("type", "npc").toLowerCase()

      val inv = inventory("inv", props)

      if (kind == "trader") {
        val prices = inventory("price", props)

        new TraderNPC(room, position, name, inv, prices)
      } else {
        val attr = prefixed("attr", props)
        val cstate = new CharacterState(room, position, name, inv, attr, new PrintStream(_ => ()))

        if (kind == "ppc") new PPC(cstate, result._2.getOrElse(""))
        else new ProgrammedNPC(cstate, result._2.getOrElse(""))
      }
  }

}
