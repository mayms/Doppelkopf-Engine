package game.engine

import game.engine.domain._
import scala.util.{Random}

import scala.actors._
import scala.actors.Actor._

case class Engine(val game:Game) {
  def playCard():Engine = {
    val (player, card) = game.player.playCard(game.table)
    val active = (game.active + 1) % 4
    val players = game.players.updated(game.active, player)
    val table = game.table :+ (game.active, card)

    return new Engine(new Game(players, active, table, game.value))
  }
  
  def showdown():Engine = {
    val players = game.table.map(_._1)
    val cards = game.table.map(_._2)

    // game.table.map((p:Player, c:Card) => (p, Game.value(c)))
    val valueTable = game.table.map((t:Tuple2[Int, Card]) => (t._1, game.value(t._2)))

    val values = valueTable.map(_._2)
    val index = values.indexWhere(_ == values.max)
    val playerIndex = players(index)

    val newPlayer = game.players(playerIndex).takeCards(cards)
    val newPlayers = game.players.updated(playerIndex, newPlayer)
    
    return new Engine(new Game(newPlayers, playerIndex, Nil, game.value))
  }
}

object Engine {
  def startGame(value: CardValue):Engine = {
    val cards = Random.shuffle(availableCards)

    val p1 = new Human("Player 1", cards.take(12), Nil)
    val p2 = new Human("Player 2", cards.drop(12).take(12), Nil)
    val p3 = new Human("Player 3", cards.drop(24).take(12), Nil)
    val p4 = new Human("Player 4", cards.drop(36).take(12), Nil)

    val game = new Game(p1 :: p2 :: p3 :: p4 :: Nil, 0, Nil, value)

    return new Engine(game)
  }

  def run(engine:Engine):Engine = {
    if(engine.game.isOver()) {
      return engine
    }
    return run(engine.playCard().playCard().playCard().playCard().showdown())
  }

  private def availableCards():List[Card] = {
    def kreuz = new Card(Ass, Kreuz) :: new Card(Koenig, Kreuz) :: new Card(Dame, Kreuz)  :: new Card(Bube, Kreuz) :: new Card(Zehn, Kreuz) :: new Card(Neun, Kreuz) :: Nil
    def pik = new Card(Ass, Pik) :: new Card(Koenig, Pik) :: new Card(Dame, Pik)  :: new Card(Bube, Pik) :: new Card(Zehn, Pik) :: new Card(Neun, Pik) :: Nil
    def hertz = new Card(Ass, Herz) :: new Card(Koenig, Herz) :: new Card(Dame, Herz)  :: new Card(Bube, Herz) :: new Card(Zehn, Herz) :: new Card(Neun, Herz) :: Nil
    def karo = new Card(Ass, Karo) :: new Card(Koenig, Karo) :: new Card(Dame, Karo)  :: new Card(Bube, Karo) :: new Card(Zehn, Karo) :: new Card(Neun, Karo) :: Nil

    return kreuz ::: kreuz ::: pik ::: pik ::: hertz ::: hertz ::: karo ::: karo
  }
}

object EngineActor {
  
  def start() {
    actor {
      val engine = Engine.startGame(NormalCardValue)
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

object TestActor {

  def start() {
    def engine = Engine.startGame(NormalCardValue)

    val cActor = self

    var ea:Actor = null
    actor {
      ea = self
      cActor ! "cont"
      a(engine)

      def a(engine: Engine) {
	react {
	  case actor:Actor => {
	    println("got msg from " + actor)
	    a(engine)
	  }
	}
      }
    }

    self.receive{case "cont" => {}}
    println("ea ready")

    var a:Actor = null
    actor {
      a = self
      cActor ! "cont"

      react {
	case "s" => {
	  println("a is sending " + self)
	  ea ! self
	}
      }
    }

    self.receive{case "cont" => {}}
    println("a ready")

    var b:Actor = null
    actor {
      b = self
      cActor ! "cont"

      react {
	case "s" => {
	  println("b is sending " + self)
	  ea ! self
	}
      }
    }

    self.receive{case "cont" => {}}
    println("b ready")

    a ! "s"
    b ! "s"
  }
}
