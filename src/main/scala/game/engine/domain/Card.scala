package game.engine.domain

case class Card(val symbol:CardSymbol, val group:CardGroup) {
  override def toString() = "Card [symbol=" + symbol + ", group=" + group + "]"
}

sealed abstract class CardSymbol {
  def apply(group: CardGroup) = Card(this, group)
}
case object Ass extends CardSymbol
case object Koenig extends CardSymbol
case object Dame extends CardSymbol
case object Bube extends CardSymbol
case object Zehn extends CardSymbol
case object Neun extends CardSymbol

sealed abstract class CardGroup {
  def apply(symbol: CardSymbol) = Card(symbol, this)
}
case object Kreuz extends CardGroup
case object Pik extends CardGroup
case object Herz extends CardGroup
case object Karo extends CardGroup
