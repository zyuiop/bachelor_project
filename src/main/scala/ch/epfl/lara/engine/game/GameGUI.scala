package ch.epfl.lara.engine.game

import java.io.{File, FileInputStream, OutputStream, PrintStream}

import javafx.beans.value.{ChangeListener, ObservableValue}
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{ScrollPane, TextField}
import scalafx.scene.image.{Image, ImageView}
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

  private lazy val imageView = new ImageView {
    preserveRatio = true
  }

  def runOnFxThread[R](op: => R): Unit = {
    if (!Platform.isFxApplicationThread) Platform.runLater(op)
    else op
  }


  override implicit val imageSetter: Option[String] => Unit = _ map (i => new File(i)) filter (_.exists) match {
    case Some(imageFile) => imageView.image = new Image(new FileInputStream(imageFile))
    case None => imageView.image = null
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
    disable = true

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
            printStream.println(Console.BOLD + "-> " + command + Console.RESET)
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
    height = 600
    onCloseRequest = () => runOnFxThread(Platform.exit())
    scene = new Scene {
      root = new BorderPane {
        padding = Insets(5)
        top = imageView
        center = new ScrollPane {
          content = textarea
          fitToWidth = true
          vvalue = 1.0
          margin = Insets(5, 0, 5, 0)

          // AutoScroll
          textarea.height.delegate.addListener(new ChangeListener[Number] {
            override def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number): Unit =
              vvalue = 1.0
          })
        }
        bottom = textfield
        imageView.fitWidth <== width.subtract(padding.value.getRight + padding.value.getLeft)
      }
    }
  }

  textfield.requestFocus()

  stage.onShown = () => new Thread(() => {
    startGame()

    runOnFxThread(textfield.disable = false)
  }).start()

  onFinishGame = () => textfield.disable = true
  onQuitGame = () => runOnFxThread(Platform.exit())
}
