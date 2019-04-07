package ch.epfl.lara.engine.game

import ch.epfl.lara.engine.game.decisions._
import ch.epfl.lara.engine.game.environment.Center

import scala.io.StdIn

/**
  * @author Louis Vialar
  */
object Game {
  def main(args: Array[String]): Unit = {
    val state = SceneState(Nil, Nil, ???, Center, None, Map(), ???)(Console.out)

    loop(state)
  }

  def loop(state: SceneState): Unit = {
    val nextStep = StdIn.readLine("> ")
    val action = Command.buildDecision(nextStep)(state.map.objects)

    action match {
      case SaveGameCommand =>
        // TODO: save game
        println("Game saved.")
        loop(state)
      case QuitGameCommand =>
        println("Do you really want to quit?")
        def confirm: Boolean = {
          StdIn.readLine("[yes/no] > ").toLowerCase match {
            case "yes" => true
            case "no" => false
            case _ => confirm
          }
        }

        if (confirm) {
          println("Goodbye!")
        } else loop(state)
      case _ => loop(state.nextState(action))
    }
  }
}
