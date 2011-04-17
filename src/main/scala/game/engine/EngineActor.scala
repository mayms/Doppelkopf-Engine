package game.engine

import scala.actors._
import scala.actors.Actor._

import game.engine.domain._

case class EngineActor(game:Game, whois:Map[PlayerId, Actor]) extends Engine {
  def getCard(player:Player, table:List[(Int, Card)]):(Player, Card) = {
    whois(player.id) ! ("play", table, self)
    receive {
      case ("played", player:Player, card:Card, actor:Actor) => {
	printf("Engine (%s): Got cmd 'played' from Player (%s)\n", self, actor)
	
	return (player, card)
      }
    }
  }
  def takeCards(player:Player, cards:List[Card]):Player = null

  def playCard() = copy(game = playCard(game))
  def showdown() = copy(game = showdown(game))
}

object EngineActor {
  
  def start(game:Game) {
    actor {
      val engine = EngineActor(game, Map.empty[PlayerId, Actor])
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
	
	val whois = engine.whois + (player.id -> actor)
	if (whois.size < 4) {
	  waiting(engine.copy(whois = whois))
	} else {
	  printf("Engine (%s): Now switching to state 'playing'\n", self)
	  playing(engine.copy(whois = whois))
	}
      }
    }
  }

  def playing(engine:EngineActor) {
    //printf("Engine (%s): Now in state 'playing'\n", self)
    val newEngine = engine.playCard()

    if (!newEngine.game.isShowdown()) {
      playing(newEngine)
    } else {
      printf("Engine (%s): Now switching to state 'calculating'\n", self)
      calculating(newEngine)
    }
  }

  def calculating(engine:EngineActor) {
    engine.game.players.foreach(player => engine.whois(player.id) ! ("exit", self))
  }
}
