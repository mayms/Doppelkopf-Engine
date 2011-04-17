package game.engine.domain

import scala.actors._
import scala.actors.Actor._

import game.engine.domain._

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
	printf("Player (%s): Got cmd 'play' from engine (%s)\n", self, engine)

	val (newPlayer, card) = player.playCard(table)
	//printf("Player (%s):\n  - newPlayer = %s\n  - card = %s\n", self, newPlayer, card)
	engine ! ("played", newPlayer, card, self)
	waiting(newPlayer)
      }

      case ("exit", engine:Actor) => {
	printf("Player (%s): Got cmd 'exit' from engine (%s)\n", self, engine)
      }
    }
  }
}
