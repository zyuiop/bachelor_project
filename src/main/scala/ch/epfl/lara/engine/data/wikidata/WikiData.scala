package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.DataQueryService
import ch.epfl.lara.engine.data.wikidata.WikiDataTypes._
import play.api.libs.json._

import scala.io.Source

/**
  * @author Louis Vialar
  */
object WikiData extends DataQueryService {

  case class WikiDataResponse(entities: Map[String, Entity])

  implicit val wikiDataResponseReads: Reads[WikiDataResponse] = Json.reads[WikiDataResponse]

  def getEntity(element: String) = {

    val json = Source.fromURL(s"https://www.wikidata.org/wiki/Special:EntityData/$element.json").mkString

    val parsed = Json.parse(json).as[WikiDataResponse]
    parsed.entities(element)
  }
}


