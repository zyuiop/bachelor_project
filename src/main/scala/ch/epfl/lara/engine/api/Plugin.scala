package ch.epfl.lara.engine.api

/**
  * @author Louis Vialar
  */
trait Plugin {
  def load(engine: Engine): Unit

  def unload(engine: Engine): Unit
}
