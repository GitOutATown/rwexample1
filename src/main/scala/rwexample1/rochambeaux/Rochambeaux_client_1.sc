package lab.rochambeaux

import Rochambeaux._

object Rochambeaux_client_1 {
  
  val mike = Player("Mike")                       //> mike  : lab.rochambeaux.Player = Player(Mike)
  val mary = Player("Mary")                       //> mary  : lab.rochambeaux.Player = Player(Mary)
  play(mike, mary)                                //> res0: String = DRAW
  play(mary, mike)                                //> res1: String = DRAW
  play(mike, mary)                                //> res2: String = DRAW
  
  for (i <- 1 until 10) println(play(mike, mary)) //> Mary
                                                  //| Mary
                                                  //| Mike
                                                  //| Mary
                                                  //| DRAW
                                                  //| DRAW
                                                  //| Mike
                                                  //| DRAW
                                                  //| Mike
}