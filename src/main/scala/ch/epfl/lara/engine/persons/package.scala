package ch.epfl.lara.engine

/**
  * @author Louis Vialar
  */
package object persons {

  case class Person(name: String, life: Timeline)

  case class Event(name: String, place: String, description: String, choices: Set[Choice])

  case class Choice(keyword: String, description: String, nextEvent: Event)

  case class Timeline(events: List[Event]) {
    def intersection(other: Timeline): Timeline = Timeline(events.intersect(other.events))

    def union(other: Timeline): Timeline = Timeline(events.union(other.events))

    /**
      * Checks if this timeline is a part of an other timeline, namely if all the events of this
      * timeline are also events in the other `main` timeline
      *
      * @param main the timeline that might contain this timeline
      * @return true if `main` contains all the events of the current timeline
      */
    def isPartOf(main: Timeline): Boolean = intersection(main) == this

    /**
      * Checks if this timeline is continuously contained in an other timeline, namely if all the
      * events of this timeline are also events in the other timeline, in the same order, and
      * with no event between two adjacent events of this timeline
      *
      * @param main the timeline that might contain this timeline
      * @return true if `main` contains continuously this timeline
      */
    def isContinuouslyPartOf(main: Timeline): Boolean = main.events.containsSlice(events)

    def addEvent(event: Event): Timeline = Timeline(event :: events)

    val lastEvent: Event = events.head

    val otherEvents: List[Event] = events.tail
  }

  object Timeline {
    def of(event: Event) = Timeline(List(event))
  }

  case class GameState(builtTimeline: Timeline, persons: Set[Person]) {
    def next(chosenEvent: Event): GameState = {
      val newTimeline = builtTimeline addEvent chosenEvent
      val newPersons = filterPersons(newTimeline)

      GameState(newTimeline, newPersons)
    }

    def availableChoices: Set[Choice] = {
      builtTimeline.lastEvent.choices.filter(choice =>
        filterPersons(builtTimeline addEvent choice.nextEvent).nonEmpty
      )
    }

    private def filterPersons(timeline: Timeline) = persons.filter(pers => timeline.isPartOf(pers.life))
  }

  case class PersonRegistry(persons: Set[Person]) {
    def startGame(startingEvent: Event): GameState =
      GameState(Timeline.of(startingEvent), persons.filter(p => p.life.events.contains(startingEvent)))
  }

  /*
  Questions :
  - do we want perfect matches only?
  - what about granularity?
   */
}
