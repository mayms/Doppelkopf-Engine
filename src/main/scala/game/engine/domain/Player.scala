package game.engine.domain

import scala.actors._
import scala.actors.Actor._

trait Player {
  val hand:List[Card]

  val won:List[Card]

  def playCard(table:List[(Int, Card)]):(Player, Card)

  def takeCards(cards:List[Card]):Player
}

case class Human(val name:String, val hand:List[Card], val won:List[Card]) extends Player {
  def playCard(table:List[(Int, Card)]) = (null, null)

  def takeCards(cards:List[Card]) = null

  override def toString() = "Human [name=" + name + ", cards=" + hand + ", won=" + won + "]"
}

object PlayerActor {
  
  def start(player: Player, engine:Actor) {
    actor {
      engine ! ("ready", player, self)
      //printf("Player (%s): Now switching to state 'waiting'\n", self)
      waiting(player)
    }
  }

  def waiting(player: Player) {
    react {
      case("play", table:List[(Int, Card)], engine:Actor) => {
	val (newPlayer, card) = player.playCard(table)
	engine ! ("played", card, self)
	waiting(newPlayer)
      }

      case ("exit", engine:Actor) => {
	printf("Player (%s): Got cmd 'exit' from engine (%s)\n", self, engine)
      }
    }
  }
}
