package ch.epfl.lara.engine.game.actions

import scala.util.Try

/**
  * This builds an action given a user input
  *
  * @author Louis Vialar
  */
trait ActionBuilder {
  /**
    * Build the action from the complete user input
    * @param input the user input, split by spaces
    * @return an optional action
    */
  def apply(input: Array[String]): Try[Action]

  /**
    * All the keywords that CAN trigger this builder
    */
  val triggeringKeywords: Set[String]
}

object ActionBuilder {
  def apply(triggering: Set[String], builder: Array[String] => Try[Action]): ActionBuilder =
    new ActionBuilder {
      override def apply(input: Array[String]): Try[Action] = builder(input)
      override val triggeringKeywords: Set[String] = triggering
    }
}