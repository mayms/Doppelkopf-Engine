package game.engine

import scala.actors._
import scala.actors.Actor._

import game.engine.domain._

case class EngineActor(game:Game) extends Engine {
  def getCard(player:Player, table:List[(Int, Card)]):(Player, Card) = null
  def takeCards(player:Player, cards:List[Card]):Player = null

  def playCard() = EngineActor(playCard(game))
  def showdown() = EngineActor(showdown(game))
}

object EngineActor {
  
  def start() {
    actor {
      val engine = EngineActor(Engine.startGame(NormalCardValue))
      engine.game.players.foreach(PlayerActor.start(_, self))
      printf("Engine (%s): Now switching to state 'waiting'\n", self)
      waiting(engine, Map.empty[Actor, Player])
    }
  }

  def waiting(engine:Engine, tmpWhois:Map[Actor, Player]) {
    //printf("Engine (%s): Now in state 'waiting'\n", self)

    react {
      case ("ready", player:Player, actor:Actor) => {
	printf("Engine (%s): Got cmd 'ready' from Player (%s)\n", self, actor)
	
	val whois = tmpWhois + (actor -> player)
	if (whois.size < 4)
	  waiting(engine, whois)
	printf("Engine (%s): Now switching to state 'playing'\n", self)
	playing(engine, whois)
      }
    }
  }

  def playing(engine:Engine, whois:Map[Actor, Player]) {
    //printf("Engine (%s): Now in state 'playing'\n", self)

    whois.keys.foreach(_ ! ("exit", self))
  }
}
