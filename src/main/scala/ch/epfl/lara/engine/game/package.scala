package ch.epfl.lara.engine

import ch.epfl.lara.engine.game.items.ItemRegistry

/**
  * @author Louis Vialar
  */
package object game {




  case class SceneMap(objects: ItemRegistry, rooms: Nothing)

  // TODO: GameState should contain the game map


  case class Scene()



  case class Person()

  // Conditions to open?


  case class Edge(source: Scene, target: Scene, activator: SceneState => Boolean)

  class ScenesGraph(vertices: Seq[Scene], edges: Seq[Edge]) {

    def findNextScene(start: Scene, decisions: SceneState): Option[Scene] = {
      edges find (e => e.source == start && e.activator(decisions)) map (_.target)
    }
  }

  /*
  Kind of stuff:
  - A scene: like a sequence
    contains
      - rooms
      - people
      - objects
      - ...
   */
}
