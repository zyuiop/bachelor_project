package ch.epfl.lara.engine.game.actions

import ch.epfl.lara.engine.game.CharacterState
import ch.epfl.lara.engine.game.messaging.Request

import scala.util.Try

/**
  * @author Louis Vialar
  */
case class ActionRequestReply(accept: Boolean, requestId: Option[Int]) extends Action {
  override def apply(inState: CharacterState): Int = {
    def act(req: Request) = {
      if (accept) req.accept()
      else req.refuse()
      3
    }

    if (requestId.isEmpty) {
      val active = inState.activeRequests
      if (active.size > 1) {
        inState.ps.println("There are multiple requests available. Please use this command again with the corresponding request id.")
        inState.ps.println("List of available requests: ")
        active.foreach(req => {
          inState.ps.println("Request id " + req.requestId + ":")
          inState.ps.println(req)
        })
        0
      } else if (active.nonEmpty) {
        val req = inState.removeAndGetRequest(active.head.requestId).get

        act(req)
      } else {
        inState.ps.println("There is no active request to " + (if (accept) "accept" else "refuse") + ".")
        0
      }
    } else {
      val req = requestId.flatMap(inState.removeAndGetRequest)

      if (req.isEmpty) {
        inState.ps.println("There is no active request to " + (if (accept) "accept" else "refuse") + ".")
        0
      } else {
        act(req.get)
      }
    }
  }
}


object ActionRequestReply extends ActionBuilder {
  override def apply(input: Array[String]): Try[Action] = Try {
    // Split parts
    val id = if (input.length > 1) {
      Some(input(1).toInt)
    } else None

    ActionRequestReply(input.head == "accept", id)
  }

  override val triggeringKeywords: Set[String] = Set("accept", "refuse")
}

