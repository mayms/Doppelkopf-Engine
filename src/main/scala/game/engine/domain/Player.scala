package game.engine.domain

trait Player {
  val hand:List[Card]

  val won:List[Card]

  def playCard(table:List[(Int, Card)]):(Player, Card)

  def takeCards(cards:List[Card]):Player
}

case class Human(name:String, hand:List[Card], won:List[Card]) extends Player {
  def playCard(table:List[(Int, Card)]) = (null, null)

  def takeCards(cards:List[Card]) = null

  override def toString() = "Human [name=" + name + ", cards=" + hand + ", won=" + won + "]"
}
