package game.engine

import game.engine.{Engine}
import game.engine.domain._

import org.scalatest.FunSuite

case class TestPlayer(id:PlayerId, hand:List[Card], won:List[Card]) extends Player {
  def this(id:String, hand:List[Card], won:List[Card]) = this(PlayerId(id), hand, won)

  def playCard(table:List[(Int, Card)]) = (copy(hand = hand.tail), hand.head)

  def takeCards(cards:List[Card]) = copy(won = cards ::: won)

  override def toString() = "TestPlayer [id=" + id + ", cards=" + hand + ", won=" + won + "]"
}

object TestCardValue extends CardValue {
  def apply(card:Card, color:CardGroup):Int = card match {
    case Card(Ass, _) => 11
    case Card(Koenig, _) => 10
    case Card(Dame, _) => 9
    case Card(Bube, _) => 8
    case Card(Zehn, _) => 7
    case Card(Neun, _) => 6
  }
}

case class TestEngine(game:Game) extends Engine {
  def getCard(player:Player, table:List[(Int, Card)]):(Player, Card) = player.playCard(table)
  def takeCards(player:Player, cards:List[Card]):Player = player.takeCards(cards)

  def playCard() = TestEngine(playCard(game))
  def showdown() = TestEngine(showdown(game))
}

class EngineTest extends FunSuite {
  private val p0 = new TestPlayer("P0", Ass(Kreuz) :: Koenig(Kreuz) :: Nil, Nil)
  private val p1 = new TestPlayer("P1", Ass(Pik) :: Koenig(Pik) :: Nil, Nil)
  private val p2 = new TestPlayer("P2", Ass(Herz) :: Koenig(Herz) :: Nil, Nil)
  private val p3 = new TestPlayer("P3", Ass(Karo) :: Koenig(Karo) :: Nil, Nil)

  private val p0c = new TestPlayer("P0", Koenig(Kreuz) :: Nil, Nil)
  private val p1c = new TestPlayer("P1", Koenig(Pik) :: Nil, Nil)
  private val p2c = new TestPlayer("P2", Koenig(Herz) :: Nil, Nil)
  private val p3c = new TestPlayer("P3", Koenig(Karo) :: Nil, Nil)

  private val p0cscs = new TestPlayer("P0", Nil, Koenig(Kreuz) :: Koenig(Pik) :: Koenig(Herz) :: Koenig(Karo) :: Ass(Kreuz) :: Ass(Pik) :: Ass(Herz) :: Ass(Karo) :: Nil)
  private val p1cscs = new TestPlayer("P1", Nil, Nil)
  private val p2cscs = new TestPlayer("P2", Nil, Nil)
  private val p3cscs = new TestPlayer("P3", Nil, Nil)

  test("Start a game.") {
    val hands = Engine.shuffle()
    hands.foreach(hand => assert(hand.size === 12))
  }

  test("Play a card.") {
    val players = p0 :: p1 :: p2 :: p3 :: Nil
    val active = 0
    var engine = TestEngine(Game(players, active, Nil, TestCardValue))
    
    engine = engine playCard

    val newPlayers =  p0c :: p1 :: p2 :: p3 :: Nil
    val newTable = (0, Ass(Kreuz)) :: Nil
    assert(engine.game === Game(newPlayers, 1, newTable, TestCardValue))
  }

  test("Play 4 cards.") {
    val players = p0 :: p1 :: p2 :: p3 :: Nil
    val active = 0
    var engine = TestEngine(Game(players, active, Nil, TestCardValue))
    
    engine = engine playCard()
    engine = engine playCard()
    engine = engine playCard()
    engine = engine playCard()

    val newPlayers =  p0c :: p1c :: p2c :: p3c :: Nil
    val newTable = (0, Ass(Kreuz)) :: (1, Ass(Pik)) :: (2, Ass(Herz)) :: (3, Ass(Karo)) :: Nil
    assert(engine.game === Game(newPlayers, 0, newTable, TestCardValue))
  }

  test("Showdown: Player0 wins.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    val table = (0, Ass(Kreuz)) :: (1, Ass(Pik)) :: (2, Ass(Herz)) :: (3, Ass(Karo)) :: Nil
    var engine = TestEngine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c.copy(won = table.map(_._2)) :: p1c :: p2c :: p3c :: Nil
    assert(engine.game === Game(newPlayers, 0, Nil, TestCardValue))
  }

  test("Showdown: Player1 wins.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    val table = (0, Koenig(Kreuz)) :: (1, Ass(Pik)) :: (2, Ass(Herz)) :: (3, Ass(Karo)) :: Nil
    var engine = TestEngine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c :: p1c.copy(won = table.map(_._2)) :: p2c :: p3c :: Nil
    assert(engine.game === Game(newPlayers, 1, Nil, TestCardValue))
  }

  test("Showdown: Player2 wins.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    val table = (0, Koenig(Kreuz)) :: (1, Koenig(Pik)) :: (2, Ass(Herz)) :: (3, Ass(Karo)) :: Nil
    var engine = TestEngine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c :: p1c :: p2c.copy(won = table.map(_._2)) :: p3c :: Nil
    assert(engine.game === Game(newPlayers, 2, Nil, TestCardValue))
  }

  test("Showdown: Player3 wins.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    val table = (0, Koenig(Kreuz)) :: (1, Koenig(Pik)) :: (2, Koenig(Herz)) :: (3, Ass(Karo)) :: Nil
    var engine = TestEngine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c :: p1c :: p2c :: p3c.copy(won = table.map(_._2)) :: Nil
    assert(engine.game === Game(newPlayers, 3, Nil, TestCardValue))
  }

  test("Game is over.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    var engine = TestEngine(Game(players, active, Nil, TestCardValue))

    engine = engine.playCard()
    engine = engine.playCard()
    engine = engine.playCard()
    engine = engine.playCard()

    engine = engine showdown

    assert(engine.game isOver)
  }
}
