package ch.epfl.lara.engine.game.actions

/**
  * The main registry for game commands
  */
object ActionsRegistry {
  private var _actionsParser: ActionParser = ActionParser()

  /**
    * Gets the actions parser of all the game actions
    */
  def actionsParser: ActionParser = _actionsParser

  /**
    * Register a new playable action in the game. This action will be available everywhere. To register an action used to
    * interact with a specific item, use the [[ActionInterceptor]] trait instead.
    * @param builders the actions to register
    */
  def registerActions(builders: ActionBuilder*): Unit = this._actionsParser = this._actionsParser.addBuilders(builders:_*)
}
