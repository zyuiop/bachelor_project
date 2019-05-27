package ch.epfl.lara.engine.game.items

import java.io.PrintStream

import ch.epfl.lara.engine.game.actions.ActionParser
import ch.epfl.lara.engine.game.entities.CharacterState

/**
  * @author Louis Vialar
  */
trait InteractableInventory extends ComplexInteractable with InventoryLike with InventoryInterceptor {


  override def printClose(implicit ps: PrintStream): Unit = super[InventoryLike].printClose



}
