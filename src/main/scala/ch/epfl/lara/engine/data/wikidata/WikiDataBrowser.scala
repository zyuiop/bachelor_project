package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.wikidata.ValueTypes._
import ch.epfl.lara.engine.data.wikidata.WikiDataTypes.{Claim, Entity, Language, Snak}
import play.api.libs.json.Json

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn


/**
  * @author Louis Vialar
  */
object WikiDataBrowser {
  implicit val language: Language = "en"
  val cache: mutable.Map[String, Entity] = mutable.Map()

  def main(args: Array[String]): Unit = {
    println("Please provide a starting page")
    val page = StdIn.readLine()
    val entity = getEntity(page)
    explore(entity)
  }

  private def getEntity(page: String) = {
    if (!cache.contains(page))
      cache.put(page, WikiData.getEntity(page))
    cache(page)
  }

  private def explore(entity: Entity): Unit = {

    println(s"You arrive at ${entity.labels(language)}... ${entity.descriptions(language)}")


    actOn(entity)

  }

  private def actOn(entity: Entity): Unit = {
    println("What do you want to do now? You can either [learn] about this place or [move] to an other place.")
    val actions = Set("move", "learn")

    @tailrec def promptAct: String = {
      print("> ")
      val act = StdIn.readLine().toLowerCase()

      if (actions(act)) act
      else {
        println("Unknown action " + act)
        promptAct
      }
    }

    val act = promptAct

    if (act == "move") moveFrom(entity)
    else if (act == "learn") learnAbout(entity)
  }

  private def learnAbout(entity: Entity) = {
    println("What do you want to learn about this place? Here are the things I know about it:")

    val propsList = entity.claims.keys.map(p => getEntity(p)).toSeq

    val requested = selectEntity(propsList)
    val value = entity.claims(requested.id)

    println(s"You wanted to learn ${requested.labels(language)} (${requested.descriptions(language)}). The answer is: ")
    printClaim(value)

    println()
    actOn(entity)
  }

  private def printClaim(claim: Seq[Claim]): Unit = {
    def printSnak(s: Snak, spaces: String): Unit = {
      print(spaces + Console.CYAN)

      s match {
        case Snak(_, _, _, None) => println("(no data...)")
        case Snak(_, _, _, Some(v)) =>
          v match {
            case EntityId(id) =>
              println(getEntity(id).labels(language))

            case GlobeCoordinate(latitude, longitude, altitude, precision, globe) =>
              val globeName = globe.id.map(getEntity).map(_.labels(language)).getOrElse(Console.RED + "(unknown)" + Console.CYAN)
              println(s"Globe coordinate: ($latitude, $longitude, $altitude) on globe $globeName")

            case MonolingualText(lang, value) =>
              if (lang == this.language)
                println(value)

            case WikiString(value) =>
              println(s"$spaces$value")

            case Quantity(amount, unitEntity) =>
              val unit = unitEntity.id.map(getEntity).map(_.labels(language)).getOrElse("(unknown unit)")
              println(spaces + s"$amount $unit")

            case UnknownValue(json, t) =>
              println(spaces + s"(unknown data type $t...) [raw: ${Json.prettyPrint(json)}]")

            case _ =>
              println(spaces + "(unknown data...)")
          }
      }

      print(Console.RESET)
    }

    def printSubClaim(c: Claim): Unit = {
      printSnak(c.mainsnak, " ")

      c.qualifiers.values.flatten.foreach(v => printSnak(v, "   "))
    }

    claim.foreach(c => printSubClaim(c))
  }

  private def moveFrom(entity: Entity) = {
    println("Where do you want to go next? Options: ")

    val neighboursList = entity.getConnectedStreets
      .map(claim => claim.mainsnak.datavalue)
      .map {
        case Some(EntityId(id)) => getEntity(id)
      }

    explore(selectEntity(neighboursList))
  }

  private def selectEntity(entities: Iterable[Entity]): Entity = {
    val entitiesPairs = entities.zipWithIndex.map(p => (p._2 + 1, p._1))
    val entitiesMap = entitiesPairs.toMap
    entitiesPairs.foreach { case (num, p) => println(s"$num: ${p.labels(language)}")}

    @tailrec def promptNum: Entity = {
      print("> ")
      val n = StdIn.readInt()

      if (entitiesMap.contains(n)) entitiesMap(n)
      else {
        println("Invalid number.")
        promptNum
      }
    }

    promptNum
  }
}
