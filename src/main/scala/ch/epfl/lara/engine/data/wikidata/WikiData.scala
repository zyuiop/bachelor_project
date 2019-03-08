package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.DataQueryService
import play.api.libs.json.Json.DefaultValues
import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json.JsonNaming.SnakeCase
import play.api.libs.json._

import scala.io.Source

/**
  * @author Louis Vialar
  */
object WikiData extends DataQueryService {

  type Language = String
  type Property = String


  case class Reference(hash: String, snaks: Map[Property, Seq[Snak]], snaksOrder: Seq[Property])


  // https://www.wikidata.org/wiki/Help:Data_type
  sealed trait Data

  implicit val dataReads: Reads[DataValue] = {
    case o@JsObject(underlying) if underlying.keySet("type") =>
      val t = (o \ "type").as[String]
      val v = o \ "value"

      val parsedValue = t match {
        case "wikibase-entityid" => v.as[EntityId]
        case "globecoordinate" | "globe-coordinate" => v.as[GlobeCoordinate]
        case "monolingualtext" => MonolingualText((v \ "language").as[String], (v \ "text").as[String])
        case _ => UnknownValue()
      }

      JsSuccess(DataValue(parsedValue, t))
    case JsObject(_) => JsError("Missing type in DataValue")
    case _ => JsError("Invalid type")
  }

  case class EntityId(entityType: String, numericId: Int, id: String) extends Data

  case class GlobeCoordinate(latitude: Double, longitude: Double, altitude: Option[Double], precision: Double, globe: String) extends Data

  case class MonolingualText(language: Language, value: String) extends Data

  // For unsupported datatypes
  case class UnknownValue() extends Data

  case class DataValue(value: Data, `type`: String)

  case class Snak(snaktype: String, property: Property, datatype: String, datavalue: DataValue)

  case class Claim(mainsnak: Snak, `type`: String, qualifiers: Map[Property, Seq[Snak]] = Map(),
                   qualifiersOrder: Seq[Property] = Seq(), id: String, rank: String, references: Seq[Reference] = Seq())

  case class Entity(`type`: String, id: String,
                    labels: Map[Language, MonolingualText],
                    descriptions: Map[Language, MonolingualText],
                    claims: Map[Property, Seq[Claim]])

  object WikiDataNaming extends JsonNaming {
    override def apply(property: String): String = SnakeCase.apply(property).replace('_', '-')
  }

  implicit val config: Aux[Json.MacroOptions with DefaultValues] = JsonConfiguration(WikiDataNaming)

  implicit val localizedStringFormat: Reads[MonolingualText] = Json.reads[MonolingualText]
  implicit val snakFormat: Reads[Snak] = Json.reads[Snak]
  implicit val referenceFormat: Reads[Reference] = Json.reads[Reference]
  implicit val claimFormat: Reads[Claim] = Json.reads[Claim]
  implicit val entityFormat: Reads[Entity] = Json.reads[Entity]
  implicit val entityIdFormat: Reads[EntityId] = Json.reads[EntityId]
  implicit val globeCoordinateFormat: Reads[GlobeCoordinate] = Json.reads[GlobeCoordinate]
  case class WikiDataResponse(entities: Map[String, Entity])

  implicit val wikiDataResponseReads: Reads[WikiDataResponse] = Json.reads[WikiDataResponse]

  def getEntity(element: String) = {

    val json = Source.fromURL(s"https://www.wikidata.org/wiki/Special:EntityData/$element.json").mkString

    val parsed = Json.parse(json).as[WikiDataResponse]
    parsed.entities(element)
  }
}


