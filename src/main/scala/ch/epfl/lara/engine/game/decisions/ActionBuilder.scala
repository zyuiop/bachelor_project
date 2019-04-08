package ch.epfl.lara.engine.game.decisions

import scala.util.Try

/**
  * This builds an action given a user input
  *
  * @author Louis Vialar
  */
trait ActionBuilder[T <: Action] {
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
  def apply[T <: Action](triggering: Set[String], builder: Array[String] => Try[Action]): ActionBuilder[T] =
    new ActionBuilder[T] {
      override def apply(input: Array[String]): Try[Action] = builder(input)
      override val triggeringKeywords: Set[String] = triggering
    }
}