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


  // https://www.wikidata.org/wiki/Help:Data_type


  case class Snak(snaktype: String, property: Property, datatype: String, datavalue: DataValue)

  case class Claim(mainsnak: Snak, `type`: String, qualifiers: Map[Property, Seq[Snak]] = Map(),
                   qualifiersOrder: Seq[Property] = Seq(), id: String, rank: String, references: Seq[Reference] = Seq())

  case class Entity(`type`: String, id: String,
                    labels: Map[Language, MonolingualText],
                    descriptions: Map[Language, MonolingualText],
                    claims: Map[Property, Seq[Claim]]) {

    def getConnectedStreets: Seq[Claim] = claims.withDefaultValue(Seq())("P47")
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
