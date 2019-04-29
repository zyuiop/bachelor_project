package ch.epfl.lara.engine.game.actions.control.compiler

import scala.annotation.tailrec


object Tokens {
  private val SymbolMap: Map[String, Token] =
    Map(
      "player" -> Player,
      "anyone" -> AnyOne,
      "someone" -> AnyOne,
      "enters" -> Enters,
      "leaves" -> Leaves,
      "talks" -> Talks,
      "interacts" -> Interacts,
      "has" -> Has)

  trait Token

  case object And extends Token

  case object Or extends Token

  case object Player extends Token

  case object AnyOne extends Token

  case object Enters extends Token

  case object Leaves extends Token

  case object Talks extends Token

  case object Interacts extends Token

  case object Has extends Token

  case object Eq extends Token

  case object Neq extends Token

  case object Lte extends Token

  case object Lt extends Token

  case object Ht extends Token

  case object Hte extends Token

  case class Symbol(name: String) extends Token

  case object Eof extends Token

  case class Error(err: String) extends Token

  case class IntLiteral(value: Int) extends Token

  case class StringLiteral(value: String) extends Token

  def read(stream: Iterable[Char]): List[Token] = {

    def strAsInt(str: String): Option[Int] = {
      if (str.nonEmpty && str.forall(_.isDigit)) Some(str.toInt)
      else if (str.forall(c => c.isDigit || c == ':')) {
        val parts = str.split(":")
        val cnt = parts(0).toInt * 3600 + parts(1).toInt * 60
        if (parts.size == 2) {
          Some(cnt)
        } else if (parts.size == 3) {
          Some(cnt + parts(2).toInt)
        } else None
      } else None
    }

    def parseSymbol(stream: Iterable[Char]): (Token, Iterable[Char]) = {
      @tailrec
      def readSymbol(stream: Iterable[Char], acc: String = ""): (String, Iterable[Char]) = {
        if (stream.isEmpty) (acc, Nil)
        else if (stream.head == ' ') (acc, stream.tail)
        else readSymbol(stream.tail, acc + stream.head)
      }

      val (text, rest) = readSymbol(stream)
      val asInt = strAsInt(text)

      val token = {
        if (asInt.nonEmpty) IntLiteral(asInt.get)
        else if (SymbolMap.contains(text.toLowerCase())) SymbolMap(text.toLowerCase())
        else if (text.forall(c => c.isLetterOrDigit || c == '_')) Symbol(text)
        else Error("Illegal caracter in symbol " + text)
      }

      (token, rest)
    }

    @tailrec
    def readStringLiteral(stream: Iterable[Char], acc: String = ""): (Token, Iterable[Char]) = {
      if (stream.isEmpty) (Error("Unterminated string literal"), Nil)
      else if (stream.head == '"') (StringLiteral(acc), stream.tail)
      else readStringLiteral(stream.tail, acc + stream.head)
    }

    def readNext(stream: Iterable[Char]): (Token, Iterable[Char]) = {
      if (stream.isEmpty) (Eof, Nil)
      else {
        val (head, tail) = (stream.head, stream.tail)

        if (head == ' ') readNext(tail)
        else if (tail.isEmpty) {
          parseSymbol(stream)
        } else {
          val next = tail.head

          (head, next) match {
            case ('=', '=') => (Eq, tail.tail)
            case ('<', '=') => (Lte, tail.tail)
            case ('<', _) => (Lt, tail)
            case ('>', '=') => (Hte, tail.tail)
            case ('>', _) => (Ht, tail)
            case ('!', '=') => (Neq, tail.tail)
            case ('&', '&') => (And, tail.tail)
            case ('|', '|') => (Or, tail.tail)
            case ('"', _) => readStringLiteral(tail)
            case _ => parseSymbol(stream)
          }
        }


      }
    }

    @tailrec
    def readLoop(stream: Iterable[Char], acc: List[Token] = List()): List[Token] = {
      if (stream.isEmpty) acc.reverse
      else {
        val (token, rest) = readNext(stream)
        readLoop(rest, token :: acc)
      }
    }

    readLoop(stream)
  }
}
