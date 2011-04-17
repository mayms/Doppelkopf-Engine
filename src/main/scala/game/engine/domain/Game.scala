package game.engine.domain

case class Game(val players:List[Player], val active:Int, val table:List[(Int, Card)], val value:CardValue) {

  /**
   * @return Spieler der am Zug ist
   */
  def player():Player = players(active)

  /**
   * @return angespielte Gruppe
   */ 
  def group():CardGroup = table(0)._2.group

  /**
   * @param card Karte
   *
   * @return Wert der Karte
   */ 
  def value(card: Card):Int = value(card, group)

  def isOver() = players.forall(p => p.hand == Nil) && table.isEmpty
}

trait CardValue {
  /**
   * @param card Karte
   * @param group angespielte Gruppe
   *
   * @return Wert der Karte
   */
  def apply(card: Card, group: CardGroup):Int
}

/**
 * Klasse zur Berechnung der Kartenwerte bei einem normalen Spiel.
 */
object NormalCardValue extends CardValue {
  def apply(card: Card, group: CardGroup) = (card, group) match {
    // ---------------------------------------------------
    // Truempfe
    // ---------------------------------------------------
    case (Card(Neun, Karo), _) => 16
  
    case (Card(Dame, Kreuz), _)  => 15
    case (Card(Dame, Pik), _)  => 14
    case (Card(Dame, Herz), _)  => 13
    case (Card(Dame, Karo), _)  => 12

    case (Card(Bube, Kreuz), _)  => 11
    case (Card(Bube, Pik), _)  => 10
    case (Card(Bube, Herz), _)  => 9
    case (Card(Bube, Karo), _)  => 8

    case (Card(Ass, Karo), _)  => 7
    case (Card(Zehn, Karo), _)  => 6
    case (Card(Koenig, Karo), _)  => 5

    // ---------------------------------------------------
    // Kreuz wurde als Erstes angespielt
    // ---------------------------------------------------
    case (Card(Ass, Kreuz), Kreuz) => 4
    case (Card(Ass, Pik), Kreuz) => 0
    case (Card(Ass, Herz), Kreuz) => 0

    case (Card(Zehn, Kreuz), Kreuz) => 3
    case (Card(Zehn, Pik), Kreuz) => 0
    case (Card(Zehn, Herz), Kreuz) => 0

    case (Card(Koenig, Kreuz), Kreuz) => 2
    case (Card(Koenig, Pik), Kreuz) => 0
    case (Card(Koenig, Herz), Kreuz) => 0

    case (Card(Neun, Kreuz), Kreuz) => 1
    case (Card(Neun, Pik), Kreuz) => 0
    case (Card(Neun, Herz), Kreuz) => 0

    // ---------------------------------------------------
    // Pik wurde als Erstes angespielt
    // ---------------------------------------------------
    case (Card(Ass, Kreuz), Pik) => 0
    case (Card(Ass, Pik), Pik) => 4
    case (Card(Ass, Herz), Pik) => 0

    case (Card(Zehn, Kreuz), Pik) => 0
    case (Card(Zehn, Pik), Pik) => 3
    case (Card(Zehn, Herz), Pik) => 0

    case (Card(Koenig, Kreuz), Pik) => 0
    case (Card(Koenig, Pik), Pik) => 2
    case (Card(Koenig, Herz), Pik) => 0

    case (Card(Neun, Kreuz), Pik) => 0
    case (Card(Neun, Pik), Pik) => 1
    case (Card(Neun, Herz), Pik) => 0

    // ---------------------------------------------------
    // Herz wurde als Erstes angespielt
    // ---------------------------------------------------
    case (Card(Ass, Kreuz), Herz) => 0
    case (Card(Ass, Pik), Herz) => 0
    case (Card(Ass, Herz), Herz) => 4

    case (Card(Zehn, Kreuz), Herz) => 0
    case (Card(Zehn, Pik), Herz) => 0
    case (Card(Zehn, Herz), Herz) => 3

    case (Card(Koenig, Kreuz), Herz) => 0
    case (Card(Koenig, Pik), Herz) => 0
    case (Card(Koenig, Herz), Herz) => 2

    case (Card(Neun, Kreuz), Herz) => 0
    case (Card(Neun, Pik), Herz) => 0
    case (Card(Neun, Herz), Herz) => 1
  }
}
