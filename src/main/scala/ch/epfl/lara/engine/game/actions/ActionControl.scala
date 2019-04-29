package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.entities.PPC
import ch.epfl.lara.engine.game.{CharacterState, GameState, PlayerState}

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionControl(characterName: String) extends Action {
  override def apply(inState: CharacterState): Int = {
    if (!inState.isInstanceOf[PlayerState]) {
      throw new IllegalStateException("This command can only be ran by real players")
    }

    val player = inState.asInstanceOf[PlayerState]

    if (player.controlled.nonEmpty) {
      println(s"You already control ${player.controlled.get.name}! Please release them before controlling someone else...")
      return 0
    }

    val characters =
      GameState.registry.getEntities(inState.currentRoom)
        .filter(_.name.toLowerCase.startsWith(characterName.toLowerCase))

    if (characters.isEmpty) {
      println("There is nobody by that name here...")
      0
    } else if (characters.size > 1) {
      println("There are too many characters by that name here... Try to use the full character name.")
      0
    } else {
      val c = characters.head

      c match {
        case ppc: PPC =>
          println(s"You concentrate all your possession powers and take control of ${ppc.name}!")
          player.control(ppc)
          5
        case _ =>
          println("You can't use your possession power on this character...")
          5
      }
    }

  }
}


object ActionControl extends ActionBuilder {

  override def apply(input: Array[String]): Try[Action] = Try {
    ActionControl(input drop 1 mkString " ")
  }

  override val triggeringKeywords: Set[String] = Set("possess", "control")
}


