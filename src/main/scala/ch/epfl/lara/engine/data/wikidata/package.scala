package ch.epfl.lara.engine.data

import ch.epfl.lara.engine.data.wikidata.WikiDataTypes.Entity

/**
  * @author Louis Vialar
  */
package object wikidata {
  trait WikiDataQueryService {
    def getEntity(id: String): Entity
  }

  trait WikiDataActionResolver {
    def describeEntity(entity: Entity): Unit

    def actOnEntity(entity: Entity): Unit
  }
}
