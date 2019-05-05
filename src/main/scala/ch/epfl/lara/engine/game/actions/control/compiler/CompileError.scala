package ch.epfl.lara.engine.game.actions.control.compiler

/**
  * @author Louis Vialar
  */
case class CompileError(location: Location, err: String) {
  override def toString: String = "Compile error: " + err + " at " + location
}

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}