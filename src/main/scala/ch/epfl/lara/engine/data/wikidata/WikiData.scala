package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.wikidata.WikiDataBrowser.cache
import ch.epfl.lara.engine.data.wikidata.WikiDataTypes._
import play.api.libs.json._

import scala.collection.mutable
import scala.io.Source

/**
  * @author Louis Vialar
  */
object WikiData extends WikiDataQueryService {

  private val cache: mutable.Map[String, Entity] = mutable.Map()

  case class WikiDataResponse(entities: Map[String, Entity])

  implicit val wikiDataResponseReads: Reads[WikiDataResponse] = Json.reads[WikiDataResponse]

  def getEntity(id: String): Entity = {
    if (!cache.contains(id)) {
      val json = Source.fromURL(s"https://www.wikidata.org/wiki/Special:EntityData/$id.json").mkString

      val parsed = Json.parse(json).as[WikiDataResponse]
      cache += (id -> parsed.entities(id))
    }

    cache(id)
  }
}


