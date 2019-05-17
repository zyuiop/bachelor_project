package ch.epfl.lara.engine.game

import javafx.scene
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontPosture, FontWeight, Text}

import scala.io.AnsiColor

/**
  * @author Louis Vialar
  */
object ColorUtils {
  private val EscapeChar = "\u001b"

  case class Format(fg: Color = null, bg: Color = null, bold: Boolean = false, underlined: Boolean = false)

  private def extractFont(line: String, format: Format): (Format, String) = {
    if (line.isEmpty || !line.contains("m")) return (format, line)

    val code = EscapeChar + line.takeWhile(_ != 'm') + "m"
    val rest = line.dropWhile(_ != 'm').tail

    val newFormat = code match {
      case AnsiColor.BLACK => format.copy(fg = Color.Black)
      case AnsiColor.RED => format.copy(fg = Color.Red)
      case AnsiColor.GREEN => format.copy(fg = Color.Green)
      case AnsiColor.YELLOW => format.copy(fg = Color.Yellow)
      case AnsiColor.BLUE => format.copy(fg = Color.Blue)
      case AnsiColor.MAGENTA => format.copy(fg = Color.Magenta)
      case AnsiColor.CYAN => format.copy(fg = Color.DarkCyan)
      case AnsiColor.WHITE => format.copy(fg = Color.White)

      /** Background **/
      case AnsiColor.BLACK_B => format.copy(bg = Color.Black)
      case AnsiColor.RED_B => format.copy(bg = Color.Red)
      case AnsiColor.GREEN_B => format.copy(bg = Color.Green)
      case AnsiColor.YELLOW_B => format.copy(bg = Color.Yellow)
      case AnsiColor.BLUE_B => format.copy(bg = Color.Blue)
      case AnsiColor.MAGENTA_B => format.copy(bg = Color.Magenta)
      case AnsiColor.CYAN_B => format.copy(bg = Color.Cyan)
      case AnsiColor.WHITE_B => format.copy(bg = Color.White)

      /** Styles **/
      case AnsiColor.RESET => format.copy(null, null, false, false)

      case AnsiColor.BOLD => format.copy(bold = true)
      case AnsiColor.UNDERLINED => format.copy(underlined = true)
      case AnsiColor.BLINK => format

      case AnsiColor.REVERSED => format
      case AnsiColor.INVISIBLE => format.copy(fg = Color.Transparent)
    }

    (newFormat, rest)
  }

  private def buildText(text: String, format: Format): Text = new Text(text) {
    if (format.fg != null)
      fill = format.fg

    if (format.bg != null)
      style = "-fx-background-color: rgb(" + format.bg.red + "," + format.bg.green + "," + format.bg.blue + ");"

    underline = format.underlined

    if (format.bold)
      font = Font.font(Font.default.family, FontWeight.Bold, Font.default.size)
  }

  def createTexts(text: String): List[Text] = {
    if (text.contains(EscapeChar)) {
      text.split(EscapeChar).foldLeft((Format(), List.empty[Text])) {
        case ((format, list), part) =>
          val (newFormat, rest) = extractFont(part, format)

          if (rest.isEmpty) (newFormat, list)
          else (newFormat, buildText(rest, newFormat) :: list)
      }._2.reverse
    } else List(new Text(text))
  }
}
