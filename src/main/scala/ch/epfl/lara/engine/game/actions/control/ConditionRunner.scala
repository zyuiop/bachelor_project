package ch.epfl.lara.engine.game.actions.control

import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.messaging.Message
import ch.epfl.lara.engine.game.messaging.Message.{RoomMovement, TalkingMessage}
import ch.epfl.lara.engine.game.{CharacterState, GameState, PlayerState}

/**
  * @author Louis Vialar
  */
object ConditionRunner {


  def runCondition(cond: Expr)(implicit runningEntity: CharacterState, trigger: Option[Message]): Boolean = {
    implicit val env: Map[String, String] = Map(
      "room" -> runningEntity.currentRoom.name,
      "totaltime" -> GameState.scheduler.currentTime.toString,
      "time" -> GameState.scheduler.dayTime.toString
    )

    cond match {
      case And(left, right) => runCondition(left) && runCondition(right)
      case Or(left, right) => runCondition(left) || runCondition(right)

      case t: Trigger => runTrigger(t)
      case c: Comparison => runComp(c)
    }
  }

  private def applies(who: Entity, actual: CharacterState): Boolean = who match {
    case AnyEntity => true
    case NamedEntity(e) => actual.name.toLowerCase == e.toLowerCase
    case PlayerEntity => actual.isInstanceOf[PlayerState]
  }

  private def runTrigger(cond: Trigger)(implicit runningEntity: CharacterState, trigger: Option[Message], env: Map[String, String]): Boolean = (cond, trigger) match {
    case (EntersTrigger(who), Some(RoomMovement(sentBy, true))) =>
      applies(who, sentBy)

    case (LeavesTrigger(who), Some(RoomMovement(sentBy, false))) =>
      applies(who, sentBy)

    case (TalksTrigger(who), Some(TalkingMessage(sentBy, _))) =>
      applies(who, sentBy)

    case (InteractsTrigger(who), _) => false // TODO: handle (and pass context...?)

    case (HasTrigger(who: Entity, what: Comparison), _) =>
      // We need to get the appropriate entities and apply the comparison on them
      val roomEntities = GameState.registry.getEntities(runningEntity.currentRoom)

      val appropriate: List[CharacterState] = who match {
        case PlayerEntity => roomEntities.filter(_.isInstanceOf[PlayerState])
        case NamedEntity(e) => roomEntities.filter(_.name.toLowerCase == e.toLowerCase)
        case AnyEntity => roomEntities
      }

      // Apply condition
      appropriate.exists(state => runComp(what)(env ++ state.attributes))

    case _ => false
  }


  private def runComp(cond: Comparison)(implicit env: Map[String, String]): Boolean = cond match {
    case Eq(left: Value, right: Value) =>
      getString(left) == getString(right)

    case Neq(left: Value, right: Value) =>
      getString(left) != getString(right)

    case Lte(left: Value, right: Value) =>
      getInt(left) <= getInt(right)

    case Lt(left: Value, right: Value) =>
      getInt(left) < getInt(right)

    case Ht(left: Value, right: Value) =>
      getInt(left) > getInt(right)

    case Hte(left: Value, right: Value) =>
      getInt(left) >= getInt(right)
  }

  private def parseIntValue(v: Value)(implicit env: Map[String, String]): Option[Int] = v match {
    case Variable(name: String) => env.get(name).filter(_.forall(_.isDigit)).map(_.toInt)
    case IntLiteral(value: Int) => Some(value)
    case StringLiteral(value: String) => Option(value).filter(_.forall(_.isDigit)).map(_.toInt)
  }

  private def getInt(v: Value)(implicit env: Map[String, String]): Int = parseIntValue(v) match {
    case Some(i) => i
    case None => throw new IllegalArgumentException("Excepted int value but got string")
  }

  private def getString(v: Value)(implicit env: Map[String, String]): String = v match {
    case Variable(name: String) => env(name)
    case IntLiteral(value: Int) => value.toString
    case StringLiteral(value: String) => value
  }
}
