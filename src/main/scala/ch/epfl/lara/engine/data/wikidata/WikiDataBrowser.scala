package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.wikidata.ValueTypes._
import ch.epfl.lara.engine.data.wikidata.WikiDataTypes.{Claim, Entity, Language, Snak}
import ch.epfl.lara.engine.data.wikidata.actions.Action
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
    val entity = WikiData.getEntity(page)
    ActionResolver.describeEntity(entity)
    ActionResolver.actOnEntity(entity)
  }

  object ActionResolver extends WikiDataActionResolver {
    override def describeEntity(entity: Entity): Unit = {
      println(s"You arrive at ${entity.labels(language)}... ${entity.descriptions(language)}")
    }

    implicit val qs: WikiDataQueryService = WikiData
    implicit val ar: WikiDataActionResolver = this

    private val actions: Map[String, Action] = Map(
      "move" -> MoveAction,
      "learn" -> LearnAction
    )

    override def actOnEntity(entity: Entity): Unit = {
      val availableActions = this.actions.filter(pair => pair._2.appliesTo(entity))
      val actions = availableActions.keySet

      @tailrec def promptAct: String = {
        val displayActions = actions.map(act => s"[$act]").mkString(" or ")
        println(s"What do you want to do now? You can either $displayActions")
        print("> ")
        val act = StdIn.readLine().toLowerCase()

        if (actions(act)) act
        else {
          println("Unknown action " + act)
          promptAct
        }
      }

      availableActions(promptAct).applyTo(entity)
    }
  }

  object MoveAction extends Action {
    /**
      * Check if the current action can be applied to a given entity
      *
      * @param entity the entity for which we want to know if the action is appliable
      * @return true if this action can be applied to the given entity
      */
    override def appliesTo(entity: Entity): Boolean = entity.hasConnectedStreets

    /**
      * Apply the current action to the given entity
      *
      * @param entity the entity to apply the action on
      * @param query  the wikidata service, used to query additional data if needed
      */
    override def applyTo(entity: Entity)(implicit query: WikiDataQueryService, resolver: WikiDataActionResolver): Unit = {
      println("Where do you want to go next? Options: ")

      val neighboursList = entity.getConnectedStreets
        .map(claim => claim.mainsnak.datavalue)
        .map {
          case Some(EntityId(id)) => query.getEntity(id)
        }

      val selected = selectEntity(neighboursList)
      resolver.describeEntity(selected)
      resolver.actOnEntity(selected)
    }
  }

  object LearnAction extends Action {
    /**
      * Check if the current action can be applied to a given entity
      *
      * @param entity the entity for which we want to know if the action is appliable
      * @return true if this action can be applied to the given entity
      */
    override def appliesTo(entity: Entity): Boolean = true

    /**
      * Apply the current action to the given entity
      *
      * @param entity the entity to apply the action on
      * @param query  the wikidata service, used to query additional data if needed
      */
    override def applyTo(entity: Entity)(implicit query: WikiDataQueryService, resolver: WikiDataActionResolver): Unit = {
      println("What do you want to learn about this thing? Here are the things I know about it:")

      val propsList = entity.claims.keys.map(p => query.getEntity(p)).toSeq

      val requested = selectEntity(propsList)
      val value = entity.claims(requested.id)

      println(s"You wanted to learn ${requested.labels(language)} (${requested.descriptions(language)}). The answer is: ")
      printClaim(value)

      println()
      resolver.actOnEntity(entity)
    }

    private def printClaim(claim: Seq[Claim]): Unit = {
      def printSnak(s: Snak, spaces: String): Unit = {
        print(spaces + Console.CYAN)

        s match {
          case Snak(_, _, _, None) => println("(no data...)")
          case Snak(_, _, _, Some(v)) =>
            v match {
              case EntityId(id) =>
                println(WikiData.getEntity(id).labels(language))

              case GlobeCoordinate(latitude, longitude, altitude, precision, globe) =>
                val globeName = globe.id.map(WikiData.getEntity).map(_.labels(language)).getOrElse(Console.RED + "(unknown)" + Console.CYAN)
                println(s"Globe coordinate: ($latitude, $longitude, $altitude) on globe $globeName")

              case MonolingualText(lang, value) =>
                if (lang == language)
                  println(value)

              case WikiString(value) =>
                println(s"$spaces$value")

              case Quantity(amount, unitEntity) =>
                val unit = unitEntity.id.map(WikiData.getEntity).map(_.labels(language)).getOrElse("(unknown unit)")
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
