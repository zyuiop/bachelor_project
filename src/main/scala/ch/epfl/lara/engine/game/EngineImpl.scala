package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.api.Engine
import ch.epfl.lara.engine.api.data.LevelParser
import ch.epfl.lara.engine.game.data.LevelParserImpl

/**
  * @author Louis Vialar
  */
class EngineImpl extends Engine {
  override def levelParser: LevelParser = LevelParserImpl
}
