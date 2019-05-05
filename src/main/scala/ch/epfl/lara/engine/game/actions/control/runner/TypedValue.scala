package ch.epfl.lara.engine.game.actions.control.runner

trait TypedValue[T] {
  val value: T

  def asString: String = value.toString
}

case class StringValue(value: String) extends TypedValue[String]

case class IntValue(value: Int) extends TypedValue[Int]

case object NullValue extends TypedValue[Null] {
  override val value: Null = null

  override def asString: String = "null"
}

case class BooleanValue(value: Boolean) extends TypedValue[Boolean]

case class UnknownTypeValue(value: String) extends TypedValue[String] {
  def canBeInt: Boolean = value.nonEmpty && value.forall(_.isDigit)

  def canBeBoolean: Boolean = value.toLowerCase == "true" || value.toLowerCase == "false"
}

case class SetValue(value: Set[String]) extends TypedValue[Set[String]]
