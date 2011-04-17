package game.engine

import game.engine.domain._
import scala.util.{Random}

trait Engine {
  def getCard(player:Player, table:List[(Int, Card)]):(Player, Card)

  def playCard():Engine

  def playCard(game:Game):Game = {
    val (player, card) = getCard(game.player, game.table)

    val active = (game.active + 1) % 4
    val players = game.players.updated(game.active, player)
    val table = game.table :+ (game.active, card)

    return new Game(players, active, table, game.value)
  }

  def takeCards(player:Player, cards:List[Card]):Player

  def showdown():Engine

  def showdown(game:Game):Game = {
    val players = game.table.map(_._1)
    val cards = game.table.map(_._2)

    // game.table.map((p:Player, c:Card) => (p, Game.value(c)))
    val valueTable = game.table.map((t:Tuple2[Int, Card]) => (t._1, game.value(t._2)))

    val values = valueTable.map(_._2)
    val index = values.indexWhere(_ == values.max)
    val playerIndex = players(index)

    val newPlayer = takeCards(game.players(playerIndex), cards)
    val newPlayers = game.players.updated(playerIndex, newPlayer)
    
    return new Game(newPlayers, playerIndex, Nil, game.value)
  }
}

object Engine {
  def shuffle():List[List[Card]] = {
    val cards = Random.shuffle(availableCards)
    return cards.take(12) :: cards.drop(12).take(12) :: cards.drop(24).take(12) :: cards.drop(36).take(12) :: Nil
  }

  private def availableCards():List[Card] = {
    def kreuz = new Card(Ass, Kreuz) :: new Card(Koenig, Kreuz) :: new Card(Dame, Kreuz)  :: new Card(Bube, Kreuz) :: new Card(Zehn, Kreuz) :: new Card(Neun, Kreuz) :: Nil
    def pik = new Card(Ass, Pik) :: new Card(Koenig, Pik) :: new Card(Dame, Pik)  :: new Card(Bube, Pik) :: new Card(Zehn, Pik) :: new Card(Neun, Pik) :: Nil
    def hertz = new Card(Ass, Herz) :: new Card(Koenig, Herz) :: new Card(Dame, Herz)  :: new Card(Bube, Herz) :: new Card(Zehn, Herz) :: new Card(Neun, Herz) :: Nil
    def karo = new Card(Ass, Karo) :: new Card(Koenig, Karo) :: new Card(Dame, Karo)  :: new Card(Bube, Karo) :: new Card(Zehn, Karo) :: new Card(Neun, Karo) :: Nil

    return kreuz ::: kreuz ::: pik ::: pik ::: hertz ::: hertz ::: karo ::: karo
  }
}
