package ch.epfl.lara.engine.game.entities

/**
  * @author Louis Vialar
  */
trait Entity {
  type State

  def runTicks(fromTime: Int, toTime: Int): Entity

  def changeState[S <: State](newState: S): Entity

  def changeStateIf[S <: State](predicate: State => Boolean, newState: S): Entity = {
    if (predicate(currentState)) changeState(newState)
    else this
  }

  val currentState: State
}
