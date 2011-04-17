package game.engine

import scala.actors._
import scala.actors.Actor._

import game.engine.domain._

case class EngineActor(game:Game, whois:Map[Player, Actor]) extends Engine {
  def getCard(player:Player, table:List[(Int, Card)]):(Player, Card) = null
  def takeCards(player:Player, cards:List[Card]):Player = null

  def playCard() = copy(game = playCard(game))
  def showdown() = copy(game = showdown(game))
}

object EngineActor {
  
  def start() {
    actor {
      val engine = EngineActor(Engine.startGame(NormalCardValue), Map.empty[Player, Actor])
      engine.game.players.foreach(PlayerActor.start(_, self))
      printf("Engine (%s): Now switching to state 'waiting'\n", self)
      waiting(engine)
    }
  }

  def waiting(engine:EngineActor) {
    //printf("Engine (%s): Now in state 'waiting'\n", self)

    react {
      case ("ready", player:Player, actor:Actor) => {
	printf("Engine (%s): Got cmd 'ready' from Player (%s)\n", self, actor)
	
	val whois = engine.whois + (player -> actor)
	if (whois.size < 4)
	  waiting(engine.copy(whois = whois))
	printf("Engine (%s): Now switching to state 'playing'\n", self)
	playing(engine.copy(whois = whois))
      }
    }
  }

  def playing(engine:EngineActor) {
    //printf("Engine (%s): Now in state 'playing'\n", self)

    engine.whois.values.foreach(_ ! ("exit", self))
  }
}
