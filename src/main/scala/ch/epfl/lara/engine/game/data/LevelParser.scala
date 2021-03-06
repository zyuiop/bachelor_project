package ch.epfl.lara.engine.game.data

import java.io.{File, PrintStream, Reader}

import ch.epfl.lara.engine.game.characters._
import ch.epfl.lara.engine.game.environment._
import ch.epfl.lara.engine.game.items.interactables.DoorItem
import ch.epfl.lara.engine.game.items.locks.InvisibleLock
import ch.epfl.lara.engine.game.items.{Interactable, Item, Storable}

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

/**
  * @author Louis Vialar
  */
object LevelParser extends RegexParsers {

  import Properties._

  private val itemTypes: mutable.Map[String, Map[String, String] => Interactable] = mutable.Map()
  private val lockTypes: mutable.Map[String, (Interactable, Map[String, String]) => Interactable] = mutable.Map()

  /**
    * Add a new item type to the parser
    *
    * @param name    the name of the item type
    * @param builder the builder, taking a map of properties and returning an item
    */
  def registerItemType(name: String)(builder: Map[String, String] => Interactable): Unit = {
    itemTypes.put(name, builder)
  }

  /**
    * Add a new lock type to the parser
    *
    * @param name    the name of the lock type
    * @param builder the builder, taking a map of properties and returning a lock
    */
  def registerLockType(name: String)(builder: (Interactable, Map[String, String]) => Interactable): Unit = {
    lockTypes.put(name, builder)
  }

  private def identifier: Parser[String] = {
    def simpleIdentifier = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

    def anyIdentifier = "[a-zA-Z0-9_]*".r ^^ { str => str }

    def spacedIdentifier = """`[^`]*`""".r ^^ { str => str drop 1 dropRight 1 }

    chainl1(simpleIdentifier | spacedIdentifier, spacedIdentifier | anyIdentifier, "." ^^^ { (s1: String, s2: String) => s1 + "." + s2 })
  }

  private def shortStringLiteral: Parser[String] = """"[^"]*"""".r ^^ { str => str drop 1 dropRight 1 }

  private def longStringLiteral: Parser[String] = "\"\"\".*\"\"\"".r ^^ { str => str drop 3 dropRight 3 replace("\\n", "\n") replace("\\t", "\t") }

  private def stringLiteral = longStringLiteral ||| shortStringLiteral

  private def intLiteral: Parser[String] = """[0-9]+""".r ^^ (v => v)

  private def keyValue = identifier ~ "=" ~! (stringLiteral | intLiteral) ^^ { case l ~ eq ~ r => (l, r) }

  private def properties = keyValue.+ ^^ { l => l.toMap }

  private def item = "[item]" ~! properties ~ lock.* ^? {
    case _ ~ props ~ locks =>
      val itemType = props("type").toLowerCase()
      val typeParser = itemTypes.get(itemType)

      if (typeParser.isEmpty)
        throw new IllegalArgumentException("unknown item type " + itemType)

      try {
        val item: Item = locks.foldLeft(typeParser.get(props))((item, builder) => builder(item))

        (props.get("position"), item) match {
          case (Some(location), i: Interactable) => Some(Position.parse(location), i)
          case (None, _: Storable) => None
          case (None, _) => throw new IllegalArgumentException("missing value position for item " + item + ", props = " + props)
        }
      } catch {
        case e: Exception => throw new IllegalArgumentException("Cannot parse item " + props + "!", e)
      }
  }

  private def lock = "[lock]" ~! properties ^? {
    case _ ~ props =>
      val lockType = props("type").toLowerCase()
      val typeParser = lockTypes.get(lockType)

      if (typeParser.isEmpty)
        throw new IllegalArgumentException("unknown lock type " + lockType)

      item: Interactable => typeParser.get.apply(item, props)
  }


  private def room = "[room]" ~ properties ~ item.* ^^ {
    case _ ~ props ~ optItems =>
      val id = props("id")

      new RoomBuilder(id) {
        override def apply(v1: List[(Position, Item with Interactable)]): Room = {
          val items = (optItems filter (_.isDefined) map (_.get)) ++ v1

          val interactables: Map[String, Map[Position, Item with Interactable]] =
            items.groupBy(_._2.displayName.toLowerCase).mapValues(_.groupBy(_._1).mapValues(_.head._2))

          val startInv = props.inventory("inv")

          new Room(id, props("name"), props("ambient"), props.get("image"), startInv, interactables)
        }
      }
  }

  private def doorType = "[doortype]" ~ properties ^^ {
    case _ ~ props =>
      val n = props("name")
      val leftToRight = props.multiVal("leftToRight")
      val rightToLeft = props.multiVal("rightToLeft")

      new DoorType(n, leftToRight, rightToLeft)
  }

  private def getTime(str: String) = {
    if (str.forall(_.isDigit)) str.toInt
    else {
      val parts = str.split(":")
      parts(0).toInt * 3600 + parts(1).toInt * 60 + parts(2).toInt
    }
  }

  private def routine = "[routine]" ~ properties ^^ {
    case _ ~ props =>
      val start = getTime(props("start"))
      val repeat = props("repeat").toInt
      val message = props("message")

      RoutineDescriptor(start, repeat, message)
  }

  private def door: Parser[DoorBuilder] = "[door]" ~ properties ^^ {
    case _ ~ props =>
      (doorTypeGetter: String => DoorType) =>
        val dt = doorTypeGetter(props("doorType"))

        val condition: (Interactable => Interactable) = if (props.contains("openCondition")) {
          item: Interactable => new InvisibleLock(item, "The door is locked", props("openCondition")).asInstanceOf[Interactable]
        } else {
          item: Interactable => item
        }

        Map(
          props("left") -> List((Position.parse(props("leftPos")), condition(new DoorItem(dt.name, props("right"), dt.leftToRight)))),
          props("right") -> List((Position.parse(props("rightPos")), condition(new DoorItem(dt.name, props("left"), if (dt.rightToLeft.isEmpty) dt.leftToRight else dt.rightToLeft))))
        )
  }

  private def program = (not(guard("[programEnd]")) ~ ".+".r).* ~! "[programEnd]" ^^ { case l ~ _ => l.map(_._2).mkString("\n") }

  private def character: Parser[CharaBuilder] = "[character]" ~ properties ~ ("[programStart]" ~! program).? ^^ {
    case _ ~ props ~ prog =>
      (rooms: String => Room) => {

        val room = rooms(props("room"))
        val name = props("name")

        val kind = props.getOrElse("type", "npc").toLowerCase()

        val inv = props.inventory("inv")

        if (kind == "trader") {
          val prices = props.inventory("price")

          new TraderNPC(room, name, inv, prices)
        } else {
          val attr = props.prefixed("attr")
          val cstate = new CharacterState(room, name, inv, attr, new PrintStream(_ => ()))

          val program = prog.map(_._2).getOrElse("")

          if (kind == "ppc") new PPC(cstate, program)
          else if (kind == "invisible") new InvisibleNPC(cstate, program)
          else new ProgrammedNPC(cstate, program)
        }
      }
  }

  private def player: Parser[PlayerBuilder] = "[player]" ~! properties ^^ {
    case _ ~ props =>
      try { (rooms: String => Room) => {
        val room = rooms(props("room"))
        val inv = props.inventory("inv")

        (ps: PrintStream, imgSetter: Option[String] => Unit) => {
          new PlayerState(room, ps, inv, imgSetter)
        }
      }
      } catch {
        case e: Exception => throw new IllegalArgumentException("Failed to instanciate player", e)
      }
  }

  private def level = "[level]" ~! properties ^^ {
    case _ ~ props =>
      // Keys
      val levelName = props("name")
      val startTime = getTime(props("startTime"))
      val currencyItem = props("currency")

      val startText = props.multiVal("startText").mkString("\n")
      val endText = props.multiVal("endText").mkString("\n")

      val levelSuccess = props("levelSuccess")
      val levelFailure = props("levelFailure")

      LevelData(Storable(currencyItem), levelName, startText, endText, levelSuccess, levelFailure, startTime)
  }

  private def file = phrase(rep(room | door | doorType | routine | character | player | level)) ^^ {
    l => {
      val (types, r1) = l.partition(_.isInstanceOf[DoorType])
      val (rooms, r2) = r1.partition(_.isInstanceOf[RoomBuilder])
      val (doors, r3) = r2.partition(_.isInstanceOf[DoorBuilder])
      val (characters, r4) = r3.partition(_.isInstanceOf[CharaBuilder])
      val (routines, r5) = r4.partition(_.isInstanceOf[RoutineDescriptor])
      val (players, levels) = r5.partition(_.isInstanceOf[PlayerBuilder])

      if (players.size != 1) throw new IllegalArgumentException("Cannot have more than 1 player descriptor")
      if (levels.size != 1) throw new IllegalArgumentException("Cannot have more than 1 level descriptor")

      val doorTypes = types.map { case t: DoorType => t.name -> t } toMap

      val mappedDoors = doors.foldLeft(Map.empty[String, List[(Position, Interactable)]])((map, obj) => {
        map ++ obj.asInstanceOf[DoorBuilder](doorTypes).map {
          case (k, v) if map.contains(k) =>
            (k, v ++ map(k))
          case (k, v) => (k, v)
        }
      })
      val reg = rooms.map(_.asInstanceOf[RoomBuilder]).map(builder => builder.roomId -> {
        builder(mappedDoors.getOrElse(builder.roomId, List()))
      }).toMap


      LevelDescriptor(reg, characters.map(_.asInstanceOf[CharaBuilder].apply(reg)),
        routines.map(_.asInstanceOf[RoutineDescriptor]), levels.head.asInstanceOf[LevelData],
        players.head.asInstanceOf[PlayerBuilder](reg))
    }
  }

  private abstract class DoorBuilder extends ((String => DoorType) => Map[String, List[(Position, Item with Interactable)]]) {}

  private abstract class PlayerBuilder extends ((String => Room) => ((PrintStream, Option[String] => Unit) => PlayerState)) {}

  private abstract class CharaBuilder extends ((String => Room) => CharacterState) {}

  private abstract class RoomBuilder(val roomId: String) extends (List[(Position, Item with Interactable)] => Room) {}

  private case class DoorType(name: String, leftToRight: List[String], rightToLeft: List[String] = Nil)

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
    val input = directory.listFiles(file => file.getName.endsWith(".txt")).map(file => {
      val src = Source.fromFile(file)
      val ret = src.getLines().mkString("\n")
      src.close()
      ret
    }).mkString("\n")

    this (input)
  }
}
