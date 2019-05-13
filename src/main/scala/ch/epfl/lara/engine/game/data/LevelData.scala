package ch.epfl.lara.engine.game.data

import ch.epfl.lara.engine.game.items.Pickable

/**
  * @author Louis Vialar
  */
case class LevelData(currency: Pickable, name: String, startText: String, endText: String, endCondition: String, startTime: Int)
