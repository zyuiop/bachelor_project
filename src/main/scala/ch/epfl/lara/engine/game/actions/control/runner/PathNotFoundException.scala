package ch.epfl.lara.engine.game.actions.control.runner

import scala.util.{Failure, Try}

/**
  * @author Louis Vialar
  */
case class PathNotFoundException(path: List[String], availablePaths: Iterable[String], cause: Exception = null) extends Exception(cause) {
  def addParentPath(parent: String): PathNotFoundException = copy(parent :: path)

  override def getMessage: String = "Path not found " + path.mkString(".") + " ; alternative paths were [" + availablePaths.mkString(", ") + "]"
}

object PathNotFoundException {
  def update(path: String): PartialFunction[Throwable, Try[TypedValue[_]]] = {
    case pne: PathNotFoundException => Failure(pne.addParentPath(path))
  }
}