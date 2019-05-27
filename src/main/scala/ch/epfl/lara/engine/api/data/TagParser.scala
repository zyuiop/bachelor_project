package ch.epfl.lara.engine.api.data

/**
  * @author Louis Vialar
  */
abstract class TagParser[TargetType](val tag: String) extends Properties {
  def build(content: Map[String, String]): TargetType
}
