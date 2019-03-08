package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.wikidata.WikiDataTypes.Language
import play.api.libs.json._

/**
  * This object contains all the supported WikiData value types
  *
  * @author Louis Vialar
  */
object ValueTypes {
  sealed trait DataValue

  /**
    * A relationship to an other entity on WikiData
    * @param id the id of the entity, allowing it to be queried from WikiData
    */
  case class EntityId(id: String) extends DataValue

  /**
    * A coordinate on a globe
    * @param latitude the latitude of the coordinate
    * @param longitude the longitude of the coordinate
    * @param altitude the altitude of the coordinate, not always present
    * @param precision the precision of the coordinate
    * @param globe the globe on which this coordinate applies
    */
  case class GlobeCoordinate(latitude: Double, longitude: Double, altitude: Option[Double], precision: Option[Double], globe: String) extends DataValue

  /**
    * A text translated in a single language
    * @param language the language of the text
    * @param value the value of the text in that language
    */
  case class MonolingualText(language: Language, value: String) extends DataValue {
    override def toString: Language = value
  }

  /**
    * A value that is not supported
    */
  case class UnknownValue() extends DataValue


  implicit val entityIdFormat: Reads[EntityId] = Json.reads[EntityId]
  implicit val globeCoordinateFormat: Reads[GlobeCoordinate] = Json.reads[GlobeCoordinate]
  implicit val localizedStringFormat: Reads[MonolingualText] = Json.reads[MonolingualText]

  /**
    * A parser for DataValues<br>
    *   WikiData's DataValues are presented this way:
    *
    * {{{
    *   {"type": "some_type", "value": { /*an object of that type*/ }}
    * }}}
    */
  implicit val dataReads: Reads[DataValue] = {
    case o@JsObject(underlying) if underlying.keySet("type") =>
      val v = o \ "value"

      val parsedValue = (o \ "type").as[String] match {
        case "wikibase-entityid" => v.as[EntityId]
        case "globecoordinate" | "globe-coordinate" => v.as[GlobeCoordinate]
        case "monolingualtext" => MonolingualText((v \ "language").as[String], (v \ "text").as[String])
        case _ => UnknownValue()
      }

      JsSuccess(parsedValue)
    case JsObject(_) => JsError("Missing type in DataValue")
    case _ => JsError("Invalid type")
  }
}
