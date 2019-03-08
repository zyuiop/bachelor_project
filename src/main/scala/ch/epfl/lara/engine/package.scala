package ch.epfl.lara

import ch.epfl.lara.engine.geometry.Interval

/**
  * @author Louis Vialar
  */
package object engine {

  case class BaseFragment(span: Interval, center: Interval) extends Fragment

  case class ComposedFragment(fragments: Seq[Fragment]) extends Fragment {
    /**
      * Return the span of this fragment.
      *
      * @return a 4D-interval representing the spatial and temporal span of this fragment (i.e. duration and area)
      */
    override def span: Interval = fragments.map(_.span).reduce(_ union _)

    /**
      * Return the center of this fragment
      *
      * @return a 4D-interval representing all the possible places and times where the center of this fragment could lye
      */
    override def center: Interval = fragments.map(_.center).reduce(_ union _)
  }

  type Property = String

  class Event(fragment: Fragment, name: String, description: String) {
    val properties: Map[Property, Seq[Fragment]] = Map()
  }

  case class BaseEvent(fragment: Fragment, name: String, description: String) extends Event(fragment, name, description)

  case class Timeline(timelineFragment: Fragment, events: Seq[Event], name: String, description: String) extends Event(timelineFragment, name, description)




  /*
  TODO : we want to define the following fragments :
    - events (i.e. cease-fire in a war)
    - persons
    - places (i.e. a building, a bridge...)
    - timelines (i.e. "history of Switzerland" or "World War 2 in Europe"...)
  Relationships between fragments might be represented using triples (fragment, property, fragment).
  Example:
    (some person's fragment, birth, the event of it's birth)
    (some person's fragment, timeline, all the events of its life)

    (an event, location, the `place` fragment about the place where the event takes place)

    (some bridge fragment, birth, the event of how this bridge was built)
    (some bridge fragment, death, the event on how this bridge was destroyed)

  Question: do we really need precise geodesic coordinates for events? In practice, if we have a location fragment that
  allows us to connect it to other locations... we don't. But it means we would have to maintain a complete modeling of
  everything that can be explored (i.e. we could not rely on the coordinates to know if two places are close, we would
  have to have a relationships that bounds them together)... which is actually maybe what we want. In the current option,
  we would probably want on a given place to get the closest registered place in 8 directions, and offer the player a
  way to reach it. But we don't know if there is a physical way to access it in the real world.

   */


}
