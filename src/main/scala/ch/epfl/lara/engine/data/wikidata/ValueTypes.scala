package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.wikidata.WikiDataTypes.Language
import play.api.libs.json._

/**
  * This object contains all the supported WikiData value types
  *
  * @author Louis Vialar
  */
object ValueTypes {
  type EntityUrl = String

  implicit class ExtendedEntityUrl(url: EntityUrl) {
    /**
      * Get the id corresponding to this entity URL
      * @return the id if found, None if not
      */
    def id: Option[String] =
      if (url.contains("/www.wikidata.org/entity")) Some(url.split("/").last)
      else None
  }

  sealed trait DataValue
  // https://www.wikidata.org/wiki/Help:Data_type

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
  case class GlobeCoordinate(latitude: Double, longitude: Double, altitude: Option[Double], precision: Option[Double], globe: EntityUrl) extends DataValue

  /**
    * A text translated in a single language
    * @param language the language of the text
    * @param value the value of the text in that language
    */
  case class MonolingualText(language: Language, value: String) extends DataValue {
    override def toString: Language = value
  }

  /**
    * A quantity of something
    * @param amount the amount of this quantity
    * @param unit the unit of this quantity
    */
  case class Quantity(amount: String, unit: EntityUrl) extends DataValue

  case class WikiString(value: String) extends DataValue {
    override def toString: Language = value
  }

  /**
    * A value that is not supported
    */
  case class UnknownValue(rawJson: JsValue, valueType: String) extends DataValue


  implicit val entityIdFormat: Reads[EntityId] = Json.reads[EntityId]
  implicit val quantityForat: Reads[Quantity] = Json.reads[Quantity]
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
      val t = (o \ "type").as[String]

      val parsedValue: DataValue = t match {
        case "wikibase-entityid" => v.as[EntityId]
        case "quantity" => v.as[Quantity]
        case "string" => WikiString(v.as[String])
        case "globecoordinate" | "globe-coordinate" => v.as[GlobeCoordinate]
        case "monolingualtext" => MonolingualText((v \ "language").as[String], (v \ "text").as[String])
        case _ => UnknownValue(v.getOrElse(JsNull), t)
      }

      JsSuccess(parsedValue)
    case JsObject(_) => JsError("Missing type in DataValue")
    case _ => JsError("Invalid type")
  }
}
