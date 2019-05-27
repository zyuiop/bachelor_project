package ch.epfl.lara.engine.api.data

/**
  * @author Louis Vialar
  */
abstract class CompoundTagParser[TargetType, ChildrenType](val tag: String) extends Properties {
  val children: Iterator[TagParser[_ <: ChildrenType]] // You cannot nest tags more than once, sorry

  def apply(content: Map[String, String], children: Iterable[ChildrenType]): TargetType
}
