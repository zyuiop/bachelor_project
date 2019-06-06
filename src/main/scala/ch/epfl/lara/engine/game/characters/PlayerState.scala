package ch.epfl.lara.engine.game.characters

import java.io.PrintStream

import ch.epfl.lara.engine.game.environment.{Center, Position, Room}
import ch.epfl.lara.engine.game.items.{InventoryLike, Storable}
import ch.epfl.lara.engine.game.messaging.Message.{ReleasedControl, TakenControl}

/**
  * @author Louis Vialar
  */
class PlayerState(startRoom: Room,
                  out: PrintStream,
                  startInventory: Map[Storable, Int] = Map(),
                  imageSetter: Option[String] => Unit = _ => ()
                 ) extends CharacterState(startRoom, "you", startInventory = startInventory, out = out) {
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

  override def inventory: InventoryLike = controlled.map(_.inventory).getOrElse(_inventory)

  override def currentRoom_=(target: Room): Unit = {
    super.currentRoom_=(target)

    imageSetter(target.image)

    controlled.foreach(_.currentRoom_=(target))
  }

  override def spawn(): Unit = {
    super.spawn()

    imageSetter(currentRoom.image)
  }

  override def changeAttribute(key: String, value: String): Unit = {
    super.changeAttribute(key, value)

    controlled.foreach(_.changeAttribute(key, value))
  }

  override def attributes: Map[String, String] = {
    super.attributes ++ controlled.map(_.attributes).getOrElse(Map())
  }
}
