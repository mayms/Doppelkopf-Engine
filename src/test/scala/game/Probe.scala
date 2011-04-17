package game

trait Verhalten {
  def doSome(p:Player):Unit
}

class BspVerhalten extends Verhalten {
  def doSome(p:Player):Unit = println(p.name)
}

case class Player(val name:String, val verhalten:Verhalten) {
  def doSome() = verhalten.doSome(this)
}

class Bla {
  def test():Unit = {
    val p = new Player("Player", new BspVerhalten())
    p.copy()
  }
}
