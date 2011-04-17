package game.engine

import game.engine.{Engine}
import game.engine.domain._

import org.scalatest.FunSuite

case class TestPlayer(val name:String, val hand:List[Card], val won:List[Card]) extends Player {
  def playCard(table:List[(Int, Card)]) = (TestPlayer(name, hand.tail, won), hand.head)

  def takeCards(cards:List[Card]) = TestPlayer(name, hand, cards ::: won)

  override def toString() = "TestPlayer [name=" + name + ", cards=" + hand + ", won=" + won + "]"
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

class EngineTest extends FunSuite {
  private val p0 = TestPlayer("P0", Ass(Kreuz) :: Koenig(Kreuz) :: Nil, Nil)
  private val p1 = TestPlayer("P1", Ass(Pik) :: Koenig(Pik) :: Nil, Nil)
  private val p2 = TestPlayer("P2", Ass(Herz) :: Koenig(Herz) :: Nil, Nil)
  private val p3 = TestPlayer("P3", Ass(Karo) :: Koenig(Karo) :: Nil, Nil)

  private val p0c = TestPlayer("P0", Koenig(Kreuz) :: Nil, Nil)
  private val p1c = TestPlayer("P1", Koenig(Pik) :: Nil, Nil)
  private val p2c = TestPlayer("P2", Koenig(Herz) :: Nil, Nil)
  private val p3c = TestPlayer("P3", Koenig(Karo) :: Nil, Nil)

  private val p0cscs = TestPlayer("P0", Nil, Koenig(Kreuz) :: Koenig(Pik) :: Koenig(Herz) :: Koenig(Karo) :: Ass(Kreuz) :: Ass(Pik) :: Ass(Herz) :: Ass(Karo) :: Nil)
  private val p1cscs = TestPlayer("P1", Nil, Nil)
  private val p2cscs = TestPlayer("P2", Nil, Nil)
  private val p3cscs = TestPlayer("P3", Nil, Nil)

  test("Start a game.") {
    val engine = Engine.startGame(TestCardValue)
    engine.game.players.foreach(p => assert(p.hand.size === 12))
  }

  test("Play a card.") {
    val players = p0 :: p1 :: p2 :: p3 :: Nil
    val active = 0
    var engine = new Engine(Game(players, active, Nil, TestCardValue))
    
    engine = engine playCard

    val newPlayers =  p0c :: p1 :: p2 :: p3 :: Nil
    val newTable = (0, Ass(Kreuz)) :: Nil
    assert(engine.game === Game(newPlayers, 1, newTable, TestCardValue))
  }

  test("Play 4 cards.") {
    val players = p0 :: p1 :: p2 :: p3 :: Nil
    val active = 0
    var engine = new Engine(Game(players, active, Nil, TestCardValue))
    
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
    var engine = new Engine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c.copy(won = table.map(_._2)) :: p1c :: p2c :: p3c :: Nil
    assert(engine.game === Game(newPlayers, 0, Nil, TestCardValue))
  }

  test("Showdown: Player1 wins.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    val table = (0, Koenig(Kreuz)) :: (1, Ass(Pik)) :: (2, Ass(Herz)) :: (3, Ass(Karo)) :: Nil
    var engine = new Engine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c :: p1c.copy(won = table.map(_._2)) :: p2c :: p3c :: Nil
    assert(engine.game === Game(newPlayers, 1, Nil, TestCardValue))
  }

  test("Showdown: Player2 wins.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    val table = (0, Koenig(Kreuz)) :: (1, Koenig(Pik)) :: (2, Ass(Herz)) :: (3, Ass(Karo)) :: Nil
    var engine = new Engine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c :: p1c :: p2c.copy(won = table.map(_._2)) :: p3c :: Nil
    assert(engine.game === Game(newPlayers, 2, Nil, TestCardValue))
  }

  test("Showdown: Player3 wins.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    val table = (0, Koenig(Kreuz)) :: (1, Koenig(Pik)) :: (2, Koenig(Herz)) :: (3, Ass(Karo)) :: Nil
    var engine = new Engine(Game(players, active, table, TestCardValue))
    
    engine = engine showdown

    val newPlayers = p0c :: p1c :: p2c :: p3c.copy(won = table.map(_._2)) :: Nil
    assert(engine.game === Game(newPlayers, 3, Nil, TestCardValue))
  }

  test("Game is over.") {
    val players = p0c :: p1c :: p2c :: p3c :: Nil
    val active = 0
    var engine = new Engine(Game(players, active, Nil, TestCardValue))

    engine = engine.playCard()
    engine = engine.playCard()
    engine = engine.playCard()
    engine = engine.playCard()

    engine = engine showdown

    assert(engine.game isOver)
  }

  test("Play until game is over.") {
    val players = p0 :: p1 :: p2 :: p3 :: Nil
    val active = 0
    var engine = new Engine(Game(players, active, Nil, TestCardValue))

    engine = Engine.run(engine)

    val newPlayers = p0cscs :: p1cscs :: p2cscs :: p3cscs :: Nil
    val newEngine = Engine(Game(newPlayers, 0, Nil, TestCardValue))
    assert(engine === newEngine)
  }
}
