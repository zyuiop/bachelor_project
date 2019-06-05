package ch.epfl.lara.engine.game.data

import java.io.{File, FileFilter, PrintStream}

import ch.epfl.lara.engine.game.entities.PlayerState

/**
  * @author Louis Vialar
  */
class LevelsManager {
  println("Loading levels...")

  private val levels: Array[LevelDescriptor] = new File("data")
    .listFiles(new FileFilter {
      override def accept(pathname: File): Boolean = pathname.isDirectory && pathname.getName.startsWith("level-")
    })
    .map(file => (file.getName.drop(6).toInt, LevelParser.readLevel(file)))
    .sortBy(_._1)
    .map(_._2)

  println("Done! Loaded " + levels.length + " levels.")

  private var currentLevel = 0

  def startNextLevel(implicit printStream: PrintStream, imageSetter: Option[String] => Unit): Option[PlayerState] = {
    currentLevel += 1
    if (levels.length < currentLevel) {
      printStream.println()
      printStream.println("Congratulations! You completed the game! Why not try it again while following an other path?")
      None
    } else {
      val level = levels(currentLevel - 1)

      Some(level.startLevel)
    }
  }

  def restartLevel(implicit printStream: PrintStream, imageSetter: Option[String] => Unit): PlayerState = {
    val level = levels(currentLevel - 1)

    level.startLevel
  }
}
