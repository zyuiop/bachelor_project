package ch.epfl.lara.engine.api

import ch.epfl.lara.engine.api.data.LevelParser

/**
  * @author Louis Vialar
  */
trait Engine {
  def levelParser: LevelParser
}

object Engine {
  var Instance: Engine = _
}
