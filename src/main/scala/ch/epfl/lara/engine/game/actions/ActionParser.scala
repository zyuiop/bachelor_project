package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.actions.general._

import scala.util.{Failure, Try}

/**
  * @author Louis Vialar
  */
class ActionParser(val actions: Map[String, Seq[ActionBuilder]]) extends ActionBuilder {
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
    val first: Try[Action] = Failure(new IllegalArgumentException("Action not found... Maybe you can't to that here?"))
    act.foldLeft(first)((tr, ab) => tr.orElse(ab.apply(input)))
  }

  /**
    * All the keywords that CAN trigger this builder
    */
  override val triggeringKeywords: Set[String] = actions.keySet

  def addBuilders(builders: ActionBuilder*): ActionParser = {
    val buildersMap = ActionParser.buildersToMap(builders)
    val nActions: Map[String, Seq[ActionBuilder]] = actions ++ buildersMap.map{ case (k, v) => (k, actions.getOrElse(k, Nil) ++ v) }

    new ActionParser(nActions)
  }

  def union(other: ActionParser): ActionParser = addBuilders(other)

  def alterResult(modifier: Try[Action] => Try[Action]): ActionParser = new ActionParser(actions) {
    override def apply(input: Array[String]): Try[Action] = modifier(super.apply(input))
  }
}

object ActionParser {

  def apply(builders: ActionBuilder*): ActionParser =
    new ActionParser(buildersToMap(builders))

  private def buildersToMap(builders: Seq[ActionBuilder]) = builders.flatMap(b => b.triggeringKeywords.map((_, b))).groupBy(_._1).mapValues(_.map(_._2))

  val DefaultParser: ActionParser = ActionParser(
    ActionUseDoor, ActionInteract, ActionWait, ActionSay, ActionTime, ActionControlStop,
    ActionControl,
    ActionGive,
    ActionRequestReply
  )
}