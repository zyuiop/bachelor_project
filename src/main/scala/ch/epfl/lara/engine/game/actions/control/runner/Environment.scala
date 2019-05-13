package ch.epfl.lara.engine.game.actions.control.runner

import java.lang.reflect.{Field, Method}

import scala.util.{Failure, Success, Try}

/**
  * @author Louis Vialar
  */

sealed trait Environment {
  def resolvePath(path: List[String]): Try[TypedValue[_]]
}

case class MapEnvironment(map: collection.Map[String, Environment]) extends Environment {
  private lazy val companion = CollectionEnvironment(map.keys.map(ValueEnvironment).toSeq)

  override def resolvePath(path: List[String]): Try[TypedValue[_]] = {
    if (path.nonEmpty) {
      map.get(path.head) match {
        case Some(v) => v.resolvePath(path.tail).recoverWith(PathNotFoundException.update(path.head))
        case None if path.head.nonEmpty && path.head.forall(_.isDigit) =>
          // Handle to the "set companion" to allow searching the keys as a set
          companion.resolvePath(path)
        case None =>
          Failure(PathNotFoundException(path.head :: Nil, map.keySet))
      }
    } else Success(SetValue(map.keySet))
  }
}

case class CollectionEnvironment(collection: Seq[Environment]) extends Environment {
  override def resolvePath(path: List[String]): Try[TypedValue[_]] = {
    if (path.nonEmpty && path.head.nonEmpty && path.head.forall(_.isDigit)) {
      val index = path.head.toInt
      if (index < collection.size) collection(index).resolvePath(path.tail).recoverWith(PathNotFoundException.update(path.head))
      else Failure(PathNotFoundException(path.head :: Nil, collection.indices.map(_.toString)))
    } else {
      Failure(PathNotFoundException(path.head :: Nil, collection.indices.map(_.toString)))
    }
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

    fieldAndMethods(obj.getClass) + ("__name", () => obj.getClass.getSimpleName)
  }

  private def produceEnv(value: Any): Environment = {
    value match {
      case map: collection.Map[_, _] => MapEnvironment(map.map(p =>
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
    if (path.nonEmpty && path.head.nonEmpty) {
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
  lazy val companion = ObjectMappingEnvironment(value)

  override def resolvePath(path: List[String]): Try[TypedValue[_]] = {
    if (path.isEmpty || path.head.isEmpty) Success(UnknownTypeValue(value))
    else companion.resolvePath(path)
  }
}

case class PassByNameEnvironment(env: () => Environment) extends Environment {
  lazy val computed: Environment = env.apply()

  override def resolvePath(path: List[String]): Try[TypedValue[_]] = computed.resolvePath(path)
}