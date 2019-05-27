package ch.epfl.lara.engine.game.data

import java.io.{File, PrintStream, Reader}

import ch.epfl.lara.engine.game.GameState
import ch.epfl.lara.engine.game.control.ActionCompiler
import ch.epfl.lara.engine.game.control.runner.ConditionExecutionContext
import ch.epfl.lara.engine.game.entities.{CharacterState, PPC, PlayerState, ProgrammedNPC, TraderNPC}
import ch.epfl.lara.engine.game.environment._
import ch.epfl.lara.engine.game.items.interactables.{BookItem, DescriptiveItem, InventoryHolderItem, Switch}
import ch.epfl.lara.engine.game.items.{Interactable, Item, Pickable}

import scala.io.Source
import scala.util.Try

/**
  * @author Louis Vialar
  */
object LevelParser extends BaseParser {

  def item = "[item]" ~! properties ^? {
    case _ ~ props =>
      val itemType = props("type").toLowerCase()
      val name = props("name")

      val item: Item with Interactable = if (itemType == "inventory") {
        val inventory = this.inventory("inv", props)

        new InventoryHolderItem(name, inventory)
      } else if (itemType == "switch") {
        val states = this.multiVal("states", props) // map states.<stateName> = transition to this state
        val transitions = this.prefixed("transitions", props) // map states.<stateName> = transition to this state
        val id = props("id")
        val time = props.get("time").flatMap(t => Try(t.toInt).toOption).getOrElse(3)

        new Switch(states, transitions, id, name, time)
      } else if (itemType == "descriptive") {
        val lore = props("lore")
        val time = props.get("time").flatMap(t => Try(t.toInt).toOption).getOrElse(3)

        new DescriptiveItem(name, lore, time)
      } else if (itemType == "book") {
        new BookItem(name, prefixed("pages", props))
      } else {
        throw new IllegalArgumentException("unknown item type " + itemType)
      }

      (props.get("position"), item) match {
        case (Some(location), i) => Some(Position.parse(location), i)
        case (None, _: Pickable) => None
        case (None, _) => throw new IllegalArgumentException("missing value position for item " + item + ", props = " + props)
      }
  }

  def room = "[room]" ~ properties ~ item.* ^^ {
    case _ ~ props ~ optItems =>
      val startInv = inventory("inv", props)

      val items = optItems filter (_.isDefined) map (_.get)

      val interactables: Map[String, Map[Position, Item with Interactable]] =
        items.groupBy(_._2.displayName).mapValues(_.groupBy(_._1).mapValues(_.head._2))

      new Room(props("id"), props("name"), props("ambient"), startInv, interactables)
  }

  def doorType = "[doortype]" ~ properties ^^ {
    case _ ~ props =>
      val n = props("name")
      val leftToRight = multiVal("leftToRight", props)
      val rightToLeft = multiVal("rightToLeft", props)

      new DoorType(n, leftToRight, rightToLeft)
  }

  private def getTime(str: String) = {
    if (str.forall(_.isDigit)) str.toInt
    else {
      val parts = str.split(":")
      parts(0).toInt * 3600 + parts(1).toInt * 60 + parts(2).toInt
    }
  }

  def routine = "[routine]" ~ properties ^^ {
    case _ ~ props =>
      val start = getTime(props("start"))
      val repeat = props("repeat").toInt
      val message = props("message")

      RoutineDescriptor(start, repeat, message)
  }

  def door: Parser[DoorBuilder] = "[door]" ~ properties ^^ {
    case _ ~ props =>
      // Keys
      val openCondition = props.get("openCondition")
        .map(ActionCompiler.compileValue)
        .map(v => new ConditionExecutionContext(v))
        .map(v => (c: CharacterState) => v.checkCondition(v.characterEnv(c)))
        .getOrElse((_: CharacterState) => true)


       (doorTypeGetter: String => DoorType) =>
        Door(props("left"), props("right"), Position.parse(props("leftPos")), Position.parse(props("rightPos")), doorTypeGetter(props("doorType")), openCondition)
  }

  def program = (not(guard("[programEnd]")) ~ ".+".r).* ~! "[programEnd]" ^^ { case l ~ _ => l.map(_._2).mkString("\n") }

  def character: Parser[CharaBuilder] = "[character]" ~ properties ~ ("[programStart]" ~! program).? ^^ {
    case _ ~ props ~ prog =>
      (rooms: String => Room) => {

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

          val program = prog.map(_._2).getOrElse("")

          if (kind == "ppc") new PPC(cstate, program)
          else new ProgrammedNPC(cstate, program)
        }
      }
  }

  def player: Parser[PlayerBuilder] = "[player]" ~! properties ^^ {
    case _ ~ props =>
      (rooms: String => Room) => {
        val room = rooms(props("room"))
        val inv = inventory("inv", props)

        (ps: PrintStream) => {
          new PlayerState(room, ps, inv)
        }
      }
  }

  def level = "[level]" ~! properties ^^ {
    case _ ~ props =>
      // Keys
      val levelName = props("name")
      val startTime = getTime(props("startTime"))
      val currencyItem = props("currency")

      val startText = multiVal("startText", props).mkString("\n")
      val endText = multiVal("endText", props).mkString("\n")

      val levelSuccess = props("levelSuccess")
      val levelFailure = props("levelFailure")

      LevelData(Pickable(currencyItem), levelName, startText, endText, levelSuccess, levelFailure, startTime)
  }

  def file = phrase(rep(room | door | doorType | routine | character | player | level)) ^^ {
    l => {
      val (types, r1) = l.partition(_.isInstanceOf[DoorType])
      val (rooms, r2) = r1.partition(_.isInstanceOf[Room])
      val (doors, r3) = r2.partition(_.isInstanceOf[DoorBuilder])
      val (characters, r4) = r3.partition(_.isInstanceOf[CharaBuilder])
      val (routines, r5) = r4.partition(_.isInstanceOf[RoutineDescriptor])
      val (players, levels) = r5.partition(_.isInstanceOf[PlayerBuilder])

      if (players.size != 1) throw new IllegalArgumentException("Cannot have more than 1 player descriptor")
      if (levels.size != 1) throw new IllegalArgumentException("Cannot have more than 1 level descriptor")

      val doorTypes = types.map { case t: DoorType => t.name -> t } toMap

      val reg = RoomRegistry(
        rooms.map(_.asInstanceOf[Room]),
        doors map { case f: DoorBuilder => f(doorTypes) })

      LevelDescriptor(reg, characters.map(_.asInstanceOf[CharaBuilder].apply(reg.getRoom)),
        routines.map(_.asInstanceOf[RoutineDescriptor]), levels.head.asInstanceOf[LevelData],
        players.head.asInstanceOf[PlayerBuilder](reg.getRoom))
    }
  }

  abstract class DoorBuilder extends ((String => DoorType) => Door) {}

  abstract class CharaBuilder extends ((String => Room) => CharacterState) {}

  abstract class PlayerBuilder extends ((String => Room) => PrintStream => PlayerState) {}

  def apply(content: String): LevelDescriptor = {
    parse(file, content) match {
      case Success(result, _) => result
      case Error(msg, _) => throw new Exception(msg)
    }
  }

  def apply(content: Reader): LevelDescriptor = {
    parse(file, content) match {
      case Success(result, _) => result
      case Error(msg, _) => throw new Exception(msg)
    }
  }

  def readLevel(directory: File): LevelDescriptor = {
    val input = directory.listFiles().map(file => {
      val src = Source.fromFile(file)
      val ret = src.getLines().mkString("\n")
      src.close()
      ret
    }).mkString("\n")

    this(input)
  }
}
