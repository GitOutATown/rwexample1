package lab.rochambeaux

case class Player(name: String) {
    def choose() = Play(Rochambeaux.randomChoice)
}

case class Play(val choice: Choice) {
    val self = this
    def compare(that: Play): Play = 
        (self.choice compare that.choice) match {
            case self.choice => self
            case that.choice => that
            case _ => Rochambeaux.draw
        }
}

trait Choice {
    val self = this
    def compare(that: Choice): Choice 
}

object ROCK extends Choice {
    def compare(that: Choice): Choice = that match {
        case PAPER => that
        case SISSORS => self
        case _ => DRAW
    }
}

object PAPER extends Choice {
    def compare(that: Choice): Choice = that match {
        case SISSORS => that
        case ROCK => self
        case _ => DRAW
    }
}

object SISSORS extends Choice {
    def compare(that: Choice): Choice = that match {
        case ROCK => that
        case PAPER => self
        case _ => DRAW
    }
}

object DRAW extends Choice {
    def compare(that: Choice) = self
}

object Rochambeaux {
    
    def play(p1: Player, p2: Player): String = {
        val p1Choice = p1.choose()
        val p2Choice = p2.choose()
        (p1Choice compare p2Choice) match {
            case `p1Choice` => p1.name
            case `p2Choice` => p2.name
            case draw => "DRAW"
        }
    }
    
    def randomChoice = scala.util.Random.shuffle(List(ROCK, PAPER, SISSORS)).head
    
    val draw = Play(DRAW)
}






