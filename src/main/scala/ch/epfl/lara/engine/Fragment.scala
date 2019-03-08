package ch.epfl.lara.engine

import ch.epfl.lara.engine.geometry.Interval

/**
  * This class represents a basic abstraction for a fragment of reality. It might be a place, an event, a person... It
  * comes with a span interval, describing how long it lasted and how large it was, and a center interval, representing
  * the uncertainty on each of the coordinates of the event.
  *
  * @author Louis Vialar
  */
trait Fragment {
  /**
    * Return the span of this fragment.
    * @return a 4D-interval representing the spatial and temporal span of this fragment (i.e. duration and area)
    */
  def span: Interval

  /**
    * Return the center of this fragment
    * @return a 4D-interval representing all the possible places and times where the center of this fragment could lye
    */
  def center: Interval
}
