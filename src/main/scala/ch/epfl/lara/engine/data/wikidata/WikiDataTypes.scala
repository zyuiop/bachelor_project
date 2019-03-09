package ch.epfl.lara.engine.data.wikidata

import play.api.libs.json.Json.DefaultValues
import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json.JsonNaming.SnakeCase
import play.api.libs.json._
import ValueTypes._

/**
  * @author Louis Vialar
  */
object WikiDataTypes {

  type Language = String
  type Property = String

  case class Reference(hash: String, snaks: Map[Property, Seq[Snak]], snaksOrder: Seq[Property])




  /**
    * A snak, i.e. a container for a value
    *
    * @param snaktype the type of the snack ("value", "novalue" ..?)
    * @param property a reminder of the name of the property
    * @param datatype the type of data (redundent)
    * @param datavalue the data value, if present
    */
  // TODO: We might want to erase this type while parsing.
  case class Snak(snaktype: String, property: Property, datatype: String, datavalue: Option[DataValue] = None)

  case class Claim(mainsnak: Snak, `type`: String, qualifiers: Map[Property, Seq[Snak]] = Map(),
                   qualifiersOrder: Seq[Property] = Seq(), id: String, rank: String, references: Seq[Reference] = Seq())

  case class Entity(`type`: String, id: String,
                    labels: Map[Language, MonolingualText],
                    descriptions: Map[Language, MonolingualText],
                    claims: Map[Property, Seq[Claim]]) {

    def getConnectedStreets: Seq[Claim] = claims.withDefaultValue(Seq())("P47")

    def toString()(implicit lang: Language): String = {
      labels.get(lang).map(_.value).getOrElse(super.toString)
    }
  }

  object WikiDataNaming extends JsonNaming {
    override def apply(property: String): String = SnakeCase.apply(property).replace('_', '-')
  }

  implicit val config: Aux[Json.MacroOptions with DefaultValues] = JsonConfiguration(WikiDataNaming)

  implicit val snakFormat: Reads[Snak] = Json.reads[Snak]
  implicit val referenceFormat: Reads[Reference] = Json.reads[Reference]
  implicit val claimFormat: Reads[Claim] = Json.reads[Claim]
  implicit val entityFormat: Reads[Entity] = Json.reads[Entity]

}
