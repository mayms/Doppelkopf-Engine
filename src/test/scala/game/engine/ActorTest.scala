package game.engine

import game.engine._
import game.engine.domain._

import org.scalatest.FunSuite
import scala.actors._
import scala.actors.Actor._

class ActorTest extends FunSuite {
  private val p0 = new TestPlayer("P0", Ass(Kreuz) :: Koenig(Kreuz) :: Nil, Nil)
  private val p1 = new TestPlayer("P1", Ass(Pik) :: Koenig(Pik) :: Nil, Nil)
  private val p2 = new TestPlayer("P2", Ass(Herz) :: Koenig(Herz) :: Nil, Nil)
  private val p3 = new TestPlayer("P3", Ass(Karo) :: Koenig(Karo) :: Nil, Nil)

  test("Start engine actor.") {
    val players = p0 :: p1 :: p2 :: p3 :: Nil
    val active = 0
    
    EngineActor.start(Game(players, active, Nil, TestCardValue))
  }
}
