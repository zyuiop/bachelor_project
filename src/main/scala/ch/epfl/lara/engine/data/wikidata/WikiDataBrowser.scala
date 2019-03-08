package ch.epfl.lara.engine.data.wikidata

import ch.epfl.lara.engine.data.wikidata.ValueTypes.EntityId
import ch.epfl.lara.engine.data.wikidata.WikiDataTypes.{Entity, Language}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

/**
  * @author Louis Vialar
  */
object WikiDataBrowser {
  val language: Language = "en"
  val cache: mutable.Map[String, Entity] = mutable.Map()

  def main(args: Array[String]): Unit = {
    println("Please provide a starting page")
    val page = StdIn.readLine()
    val entity = getEntity(page)
    explore(entity)
  }

  private def getEntity(page: String) = {
    if (!cache.contains(page))
      cache.put(page, WikiData.getEntity(page))
    cache(page)
  }

  private def explore(entity: Entity): Unit = {

    println(s"You arrive at ${entity.labels(language)}... ${entity.descriptions(language)}")

    val neighboursList = entity.getConnectedStreets
      .map(claim => claim.mainsnak.datavalue)
      .map {
        case Some(EntityId(id)) => getEntity(id)
      }
      .zipWithIndex.map(p => (p._2 + 1, p._1))

    val neighbours = neighboursList.toMap

    println("Where do you want to go next? Options: ")
    neighboursList.foreach { case (num, p) => println(s"$num: ${p.labels(language)}")}

    @tailrec def promptNum: Entity = {
      print("> ")
      val n = StdIn.readInt()

      if (neighbours.contains(n)) neighbours(n)
      else {
        println("Invalid number.")
        promptNum
      }
    }

    explore(promptNum)
  }
}
