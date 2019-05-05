package ch.epfl.lara.engine.game.actions.control

import java.lang.reflect.{Field, Method}

import ch.epfl.lara.engine.game.actions.control.compiler.Tree._
import ch.epfl.lara.engine.game.messaging.Message
import ch.epfl.lara.engine.game.{CharacterState, GameState}

import scala.util.{Failure, Success, Try}

/**
  * @author Louis Vialar
  */
object ConditionRunner {

  private case class PathNotFoundException(path: List[String], availablePaths: Iterable[String], cause: Exception = null) extends Exception(cause) {
    def addParentPath(parent: String): PathNotFoundException = copy(parent :: path)

    override def getMessage: String = "Path not found " + path.mkString(".") + " ; alternative paths were [" + availablePaths.mkString(", ") + "]"
  }

  private object PathNotFoundException {
    def update(path: String): PartialFunction[Throwable, Try[TypedValue[_]]] = {
      case pne: PathNotFoundException => Failure(pne.addParentPath(path))
    }
  }

  sealed trait Environment {
    def resolvePath(path: List[String]): Try[TypedValue[_]]
  }

  case class MapEnvironment(map: Map[String, Environment]) extends Environment {
    override def resolvePath(path: List[String]): Try[TypedValue[_]] = {
      if (path.nonEmpty) {
        map.get(path.head) match {
          case Some(v) => v.resolvePath(path.tail).recoverWith(PathNotFoundException.update(path.head))
          case None => Failure(PathNotFoundException(path.head :: Nil, map.keySet))
        }
      } else Success(SetValue(map.keySet))
    }
  }

  case class ObjectMappingEnvironment(obj: Any) extends Environment {

    private lazy val fieldsAndMethods = {
      def readField(f: Field) = () => {
        if (!f.isAccessible) f.setAccessible(true)
        f.get(obj)
      }
      def readMethod(m: Method) = () => {
        if (!m.isAccessible) m.setAccessible(true)
        m.invoke(obj)
      }

      def fieldAndMethods(c: Class[_]): Map[String, () => AnyRef] = {
        if (c == null) Map()
        else {
          c.getDeclaredFields.map(f => (f.getName, readField(f))).toMap ++
            c.getDeclaredMethods.filter(_.getParameterCount == 0).map(m => (m.getName, readMethod(m))).toMap ++
            fieldAndMethods(c.getSuperclass)
        }
      }

      fieldAndMethods(obj.getClass)
    }

    private def produceEnv(value: Any): Environment = {
      value match {
        case map: Map[_, _] => MapEnvironment(map.map(p =>
          p._1.toString -> produceEnv(p._2)
        ))
        case s: String => ValueEnvironment(s)
        case i: Int => ValueEnvironment(i.toString)
        case d: Double => ValueEnvironment(d.toString)
        case b: Boolean => ValueEnvironment(b.toString)
        case _ => ObjectMappingEnvironment(value)
      }
    }


    override def resolvePath(path: List[String]): Try[TypedValue[_]] = {
      if (path.nonEmpty) {
        val optField = fieldsAndMethods.get(path.head)

        if (optField.nonEmpty) {
          produceEnv(optField.get.apply()).resolvePath(path.tail).recoverWith(PathNotFoundException.update(path.head))
        } else {
          Failure(PathNotFoundException(path.head :: Nil, fieldsAndMethods.keySet))
        }
      } else Success(SetValue(fieldsAndMethods.keySet))
    }
  }

  case class ValueEnvironment(value: String) extends Environment {
    override def resolvePath(path: List[String]): Try[TypedValue[_]] = {
      if (path.isEmpty) Success(UnknownTypeValue(value))
      else Failure(PathNotFoundException(path.head :: Nil, Set()))
    }
  }

  case class PassByNameEnvironment(env: () => Environment) extends Environment {
    override def resolvePath(path: List[String]): Try[TypedValue[_]] = env.apply().resolvePath(path)
  }


  def runCondition(cond: LogicalExpression)(implicit runningEntity: CharacterState, trigger: Option[Message]): Boolean = {
    implicit val env: Environment = MapEnvironment(Map(
      "time" -> ValueEnvironment(GameState.scheduler.dayTime.toString),
      "totalTime" -> ValueEnvironment(GameState.scheduler.currentTime.toString),
      "characters" -> PassByNameEnvironment(() => MapEnvironment(
        GameState.registry.getEntities(runningEntity.currentRoom).map(state => (state.name, ObjectMappingEnvironment(state)))
          .toMap + ("me" -> ObjectMappingEnvironment(runningEntity)) + ("player" -> ObjectMappingEnvironment(GameState.registry.player))
      )),
      "room" -> PassByNameEnvironment(() => ObjectMappingEnvironment(runningEntity.currentRoom)),
      "state" -> PassByNameEnvironment(() => ObjectMappingEnvironment(GameState)),
      "trigger" ->
        trigger.map(m => MapEnvironment(Map(
          "type" -> ValueEnvironment(m.getClass.getSimpleName),
          "content" -> ObjectMappingEnvironment(m)
        ))).getOrElse(MapEnvironment(Map("type" -> ValueEnvironment("None"))))
    ))

    def recRunCondition(cond: LogicalExpression): Boolean = {

      cond match {
        case And(l, r) => recRunCondition(l) && recRunCondition(r)
        case Or(l, r) => recRunCondition(l) || recRunCondition(r)
        case Not(e) => !recRunCondition(e)
        case c: Comparison => checkComparison(c)
      }
    }

    recRunCondition(cond)
  }


  trait TypedValue[T] {
    val value: T

    def asString: String = value.toString
  }

  case class StringValue(value: String) extends TypedValue[String]

  case class IntValue(value: Int) extends TypedValue[Int]

  case class BooleanValue(value: Boolean) extends TypedValue[Boolean]

  case class UnknownTypeValue(value: String) extends TypedValue[String] {
    def canBeInt: Boolean = value.nonEmpty && value.forall(_.isDigit)

    def canBeBoolean: Boolean = value.toLowerCase == "true" || value.toLowerCase == "false"
  }

  case class SetValue(value: Set[String]) extends TypedValue[Set[String]]


  def resolve(value: Value)(implicit env: Environment): TypedValue[_] = value match {
    case Concat(left, right) => (resolve(left), resolve(right)) match {
      case (SetValue(l1), SetValue(l2)) => SetValue(l1 ++ l2)
      case (SetValue(lst), v) => SetValue(lst + v.asString)
      case (v, SetValue(lst)) => SetValue(lst + v.asString)
      case (l, r) => StringValue(l.asString + r.asString)
    }
    case StringLiteral(s) => StringValue(s)
    case BooleanLiteral(b) => BooleanValue(b)
    case IntLiteral(i) => IntValue(i)
    case Identifier(parts) =>
      env.resolvePath(parts).get
  }


  def resolveAsNumber(value: Value)(implicit env: Environment): Int = resolve(value) match {
    case IntValue(i) => i
    case u@UnknownTypeValue(v) if u.canBeInt => v.toInt
    case _ => throw new IllegalArgumentException(value + " cannot be considered as an int!")
  }

  def checkComparison(condition: Comparison)(implicit env: Environment): Boolean = condition match {
    case BooleanLiteral(value) => value
    case Eq(left: Value, right: Value) =>
      resolve(left).asString == resolve(right).asString
    case Neq(left: Value, right: Value) =>
      resolve(left).asString != resolve(right).asString
    case Lte(left: Value, right: Value) =>
      resolveAsNumber(left) <= resolveAsNumber(right)
    case Lt(left: Value, right: Value) =>
      resolveAsNumber(left) < resolveAsNumber(right)
    case Ht(left: Value, right: Value) =>
      resolveAsNumber(left) > resolveAsNumber(right)
    case Hte(left: Value, right: Value) =>
      resolveAsNumber(left) >= resolveAsNumber(right)
    case In(left: Value, right: Value) =>
      val l = resolve(left)
      val r = resolve(right)

      r match {
        case StringValue(_) | UnknownTypeValue(_) =>
          l match {
            case StringValue(_) | UnknownTypeValue(_) =>
              r.asString.contains(l.asString)

            case _ =>
              throw new IllegalArgumentException(left + " cannot be at the left hand side of a `in` operator if the right hand is a string")
          }

        case SetValue(rightSet) =>

          l match {
            case SetValue(leftSet) => leftSet.forall(s => rightSet(s))
            case _ => rightSet(l.asString)
          }
        case _ => throw new IllegalArgumentException("Illegal use of in operator with right hand " + r)
      }
  }
}
