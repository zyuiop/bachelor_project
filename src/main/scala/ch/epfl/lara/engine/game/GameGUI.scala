package ch.epfl.lara.engine.game

import java.io.{OutputStream, PrintStream}

import javafx.beans.value.{ChangeListener, ObservableValue}
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{ScrollPane, TextField}
import scalafx.scene.image.ImageView
import scalafx.scene.input.KeyCode
import scalafx.scene.layout.BorderPane
import scalafx.scene.text.{Text, TextAlignment, TextFlow}

import scala.collection.mutable

/**
  * @author Louis Vialar
  */
object GameGUI extends Game with JFXApp {
  private lazy val textarea = new TextFlow {
    textAlignment = TextAlignment.Left
  }

  def runOnFxThread[R](op: => R): Unit = {
    if (!Platform.isFxApplicationThread) Platform.runLater(op)
    else op
  }

  override val printStream: PrintStream = new PrintStream(new OutputStream {

    private val sb: mutable.StringBuilder = new mutable.StringBuilder()

    override def write(b: Int): Unit = {
      val c = b.toChar
      sb += c

      if (c == '\n') {
        val t = sb.toString
        sb.clear()

        val texts = ColorUtils.createTexts(t)

        runOnFxThread(texts.foreach(t => textarea.children.add(t.delegate)))
      }
    }
  })

  private val textfield = new TextField {
    promptText = "Command..."

    onKeyPressed = { ev =>
      ev.code match {
        case KeyCode.Down =>
          historyDown() match {
            case Some(com) => text.value = com
            case None => text.value = ""
          }
        case KeyCode.Up =>
          historyUp() match {
            case Some(com) => text.value = com
            case None => // Do nothing
          }
        case KeyCode.Enter if text.value.nonEmpty =>
          val command = text.value
          addToHistory(command)
          text.value = ""
          disable = true

          new Thread(() => {
            runCommand(command)
            runOnFxThread {
              disable = false
              requestFocus()
            }
          }).start()
        case _ =>
      }
    }
  }

  private val history = mutable.ArrayStack[String]()
  private var historyPos = 0

  private def addToHistory(command: String) = {
    history.push(command)
    historyPos = 0
  }

  private def historyUp(): Option[String] = {
    if (history.size > historyPos) {
      val ret = history(historyPos)
      historyPos += 1
      Some(ret)
    } else None
  }

  private def historyDown(): Option[String] = {
    if (historyPos > 0) {
      historyPos -= 1
      val ret = history(historyPos)
      Some(ret)
    } else None
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "The Game"
    width = 600
    height = 450
    onCloseRequest = () => runOnFxThread(Platform.exit())
    scene = new Scene {
      root = new BorderPane {
        padding = Insets(5)
        top = new ImageView
        center = new ScrollPane {
          content = textarea
          fitToWidth = true
          vvalue = 1.0

          // AutoScroll
          textarea.height.delegate.addListener(new ChangeListener[Number] {
            override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit =
              vvalue = 1.0
          })
        }
        bottom = textfield
      }
    }
  }

  textfield.requestFocus()

  startGame()

  onFinishGame = () => textfield.disable = true
  onQuitGame = () => runOnFxThread(Platform.exit())
}
