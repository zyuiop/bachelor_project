package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.entities.PPC
import ch.epfl.lara.engine.game.environment.{Center, Position, Room}
import ch.epfl.lara.engine.game.items.{Inventory, Pickable}

/**
  * @author Louis Vialar
  */
class PlayerState(startRoom: Room) extends CharacterState(startRoom, Center, "you") {
  private var _controlled: Option[PPC] = None

  def controlled: Option[PPC] = _controlled

  private def controlledInv: Option[Inventory] = controlled.map(_.inventory)

  private val selfInv = _inventory

  def control(ppc: PPC): Unit = {
    _controlled = Some(ppc)
    ppc.takeControl()
  }

  def release(): Unit = {
    _controlled.get.releaseControl()
    _controlled = None
  }

  override def inventory: Inventory = new Inventory {
    override val name: String = selfInv.name

    override def getContent: Map[Pickable, Int] = {
      if (controlledInv.nonEmpty) {
        val selfContent = selfInv.getContent
        val controlledInvContent =
          controlledInv.get.getContent.map(pair => {
            if (selfContent.contains(pair._1))
              (pair._1, selfContent(pair._1) + pair._2)
            else pair
          })

        selfContent ++ controlledInvContent
      } else {
        selfInv.getContent
      }
    }

    override def take(o: Pickable, quantity: Int): Inventory = {
      if (controlledInv.nonEmpty && controlledInv.get.canTake(o, quantity))
        controlled.get.inventory.take(o, quantity)
      else selfInv.take(o, quantity)
      this
    }

    override def add(o: Pickable, quantity: Int): Inventory = {
      if (controlledInv.nonEmpty)
        controlledInv.get.add(o, quantity)
      else selfInv.add(o, quantity)
    }

    override def canTake(o: Pickable, quantity: Int): Boolean = {
      (controlledInv.nonEmpty && controlledInv.get.canTake(o, quantity)) || selfInv.canTake(o, quantity)
    }
  }

  override def changeRoom(target: Room): Unit = {
    super.changeRoom(target)

    controlled.foreach(_.changeRoom(target))
  }

  override def changePosition(target: Position): Unit = {
    super.changePosition(target)

    controlled.foreach(_.changePosition(target))
  }

  override def changeAttribute(key: String, value: String): Unit = {
    super.changeAttribute(key, value)

    controlled.foreach(_.changeAttribute(key, value))
  }

  override def attributes: Map[String, String] = {
    super.attributes ++ controlled.map(_.attributes).getOrElse(Map())
  }
}
