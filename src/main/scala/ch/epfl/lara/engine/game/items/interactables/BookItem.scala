package ch.epfl.lara.engine.game.items.interactables

import java.io.PrintStream

import ch.epfl.lara.engine.game.items.{ComplexInteractable, Storable}

/**
  * @author Louis Vialar
  */
class BookItem(override val displayName: String, pages: Map[String, String]) extends Storable with ComplexInteractable {
  handle("open", "go", "read") { (state, args) =>
    val sectionName = (if (args.head == "to") args drop 1 else args) mkString " "

    val (section, time) = pages
      .get(sectionName.toLowerCase)
      .map(content => (s"Section $sectionName reads:\n$content\n", content.length / 5))
      .getOrElse(("There is no such section...", 15)) // it takes time to search for a section that doesn't exist :o

    state.ps.println(section)
    time
  }

  handle("list", "contents", "table") { (state, _) =>
    printTableOfContents(state.ps)
    4
  }

  private def printTableOfContents(printStream: PrintStream): Unit = {
    printStream.println("It contains the following sections: " + pages.keys.mkString("; "))
  }

  override def printClose(implicit ps: PrintStream): Unit = {
    ps.println(s"You close the $displayName")
  }

  override def printOpen(implicit ps: PrintStream): Unit = {
    ps.println(s"You open the $displayName")
    printTableOfContents(ps)
  }
}
