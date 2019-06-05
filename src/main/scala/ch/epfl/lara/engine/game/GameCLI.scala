package ch.epfl.lara.engine.game

import java.io.PrintStream

import scala.io.StdIn

/**
  * @author Louis Vialar
  */
object GameCLI extends Game {
  override val printStream: PrintStream = Console.out
  override implicit val imageSetter: Option[String] => Unit = _ => ()

  def main(args: Array[String]): Unit = {
    startGame()

    while (running) {
      runCommand(StdIn.readLine("> "))
    }
    printStream.println("Good bye!")
  }

  private var running: Boolean = true

  def quitGame(): Unit = {
    running = false
  }

  onFinishGame = quitGame
  onQuitGame = quitGame
}
