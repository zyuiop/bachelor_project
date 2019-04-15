package ch.epfl.lara.engine.game

/**
  * @author Louis Vialar
  */
class GameState() {
  GameState.instance = Some(this)




  // TODO: wrap LevelState, and detect scene switches
  // do that only once SceneStates work correctly
}

object GameState {
  private var instance: Option[GameState] = None

  def get: GameState = instance.get
}