package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.entities.PPC
import ch.epfl.lara.engine.game.environment.{Center, Position, Room}
import ch.epfl.lara.engine.game.items.{Inventory, Pickable}
import ch.epfl.lara.engine.game.messaging.Message.{ReleasedControl, TakenControl}

/**
  * @author Louis Vialar
  */
class PlayerState(startRoom: Room, startInventory: Map[Pickable, Int] = Map()) extends CharacterState(startRoom, Center, "you", startInventory = startInventory) {
  private var _controlled: Option[PPC] = None

  def controlled: Option[PPC] = _controlled

  def control(ppc: PPC): Unit = {
    _controlled = Some(ppc)
    ppc ! TakenControl(this)
  }

  def release(): Unit = {
    _controlled.get ! ReleasedControl(this)
    _controlled = None
  }

  override def inventory: Inventory = controlled.map(_.inventory).getOrElse(_inventory)

  override def currentRoom_=(target: Room): Unit = {
    super.currentRoom_=(target)

    controlled.foreach(_.currentRoom_=(target))
  }

  override def currentPosition_=(target: Position): Unit = {
    super.currentPosition_=(target)

    controlled.foreach(_.currentPosition = target)
  }

  override def changeAttribute(key: String, value: String): Unit = {
    super.changeAttribute(key, value)

    controlled.foreach(_.changeAttribute(key, value))
  }

  override def attributes: Map[String, String] = {
    super.attributes ++ controlled.map(_.attributes).getOrElse(Map())
  }
}
