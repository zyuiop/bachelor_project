package ch.epfl.lara.engine.game.actions

import scala.util.{Failure, Try}

/**
  * @author Louis Vialar
  */
class ActionParser(val actions: Map[String, Seq[ActionBuilder[_ <: Action]]]) extends ActionBuilder[Action] {
  /**
    * Build the action from the complete user input
    *
    * @param input the user input, split by spaces
    * @return an optional action
    */
  override def apply(input: Array[String]): Try[Action] = {
    if (input.isEmpty)
      return Failure(new IllegalArgumentException("empty command"))

    val keyword = input(0).toLowerCase()
    val act = actions.getOrElse(keyword, Nil)
    val first: Try[Action] = Failure(new IllegalArgumentException("command not found"))
    act.foldLeft(first)((tr, ab) => tr.orElse(ab.apply(input)))
  }

  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = actions.keySet

  def addBuilders(builders: ActionBuilder[_ <: Action]*): ActionParser = {
    val buildersMap = ActionParser.buildersToMap(builders)
    val nActions: Map[String, Seq[ActionBuilder[_ <: Action]]] = actions ++ buildersMap.map{ case (k, v) => (k, actions.getOrElse(k, Nil) ++ v) }

    new ActionParser(nActions)
  }

  def union(other: ActionParser): ActionParser = addBuilders(other)
}

object ActionParser {

  def apply(builders: ActionBuilder[_ <: Action]*): ActionParser =
    new ActionParser(buildersToMap(builders))

  private def buildersToMap(builders: Seq[ActionBuilder[_ <: Action]]) = builders.flatMap(b => b.triggeringKeywords.map((_, b))).groupBy(_._1).mapValues(_.map(_._2))

  val DefaultParser: ActionParser = ActionParser(
    ActionUseDoor, ActionInteract, ActionWait
  )
}