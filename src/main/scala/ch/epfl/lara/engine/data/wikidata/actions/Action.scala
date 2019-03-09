package ch.epfl.lara.engine.data.wikidata.actions

import ch.epfl.lara.engine.data.wikidata.{WikiDataActionResolver, WikiDataQueryService}
import ch.epfl.lara.engine.data.wikidata.WikiDataTypes.Entity

/**
  * @author Louis Vialar
  *
  *         An action is something that can be done with a specific entity
  */
trait Action {
  /**
    * Check if the current action can be applied to a given entity
    *
    * @param entity the entity for which we want to know if the action is appliable
    * @return true if this action can be applied to the given entity
    */
  def appliesTo(entity: Entity): Boolean

  /**
    * Apply the current action to the given entity
    *
    * @param entity the entity to apply the action on
    * @param query the wikidata service, used to query additional data if needed
    */
  def applyTo(entity: Entity)(implicit query: WikiDataQueryService, resolver: WikiDataActionResolver): Unit
}

