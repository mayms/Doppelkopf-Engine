package game.engine

import game.engine._
import game.engine.domain._

import org.scalatest.FunSuite
import scala.actors._
import scala.actors.Actor._

class ActorTest extends FunSuite {
  test("Start engine actor.") {
    EngineActor.start()
    //EngineActor.start() ! ("start", NormalCardValue)
  }
}
