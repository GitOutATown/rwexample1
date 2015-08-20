package rwexample1.model.population

import scala.util.Random

object PopulationDrift {
    
    trait Marble
    case object Black extends Marble
    case object White extends Marble
    
    def ratio(group: Seq[Marble]): (Double, Double) = {
        val black = group.filter(m => m == Black)
        val ratioBlk = black.length.toDouble / group.length.toDouble
        val ratioWht = 1d - ratioBlk
        (ratioBlk, ratioWht)
    }
    
    /* This algorithm introduces some degree of stohasticism in that the cycle of duplication
     * does not evenly fit the limit (i.e. 100, 200, 400, 800 -> 1000) so that the last
     * remainder of increase is randomly chosen (i.e. does not adhere to the original input
     * ratio.
     * The idea was to stick close to a mechanical increase mechanism as opposed to a pure
     * mathematical projection.
     * Recursive.
     * TODO: Also implement a purely mathematical projection of the original ratio projected
     * to the limit.
     */
    def duplicate(limit: Int)(group: Seq[Marble]): Seq[Marble] = {
        group.length match {
            case l if (l <= (limit / 2)) => {
                val result = group.flatMap(m => m match {
                    case Black => Seq(Black, Black)
                    case White => Seq(White, White)
                })
                duplicate(limit)(result)
            }
            case l if (limit - l < 2) => group
            case _ => {
                val lastLimit = limit - group.length
                val lastLength = lastLimit / 2
                group ++ duplicate(lastLimit)(Random.shuffle(group).take(lastLength))
            }
        }
    }
    
    /* Cycle reproduction of successive population samples. Recursive
     */
    def cycleSampleRepro(maxNumCycles: Int)
                        (sampleSize: Int)
                        (reproLim: Int) // reproduction size cap
                        (group: Seq[Marble]): Seq[Marble] = {
        
        def inter(group: Seq[Marble], cyclesRemain: Int): Seq[Marble] = {
            val sample = Random.shuffle(group).take(sampleSize)
            val newGroup = duplicate(reproLim)(sample)
            val popRatio = ratio(newGroup)
            println("==> Cycle: " + (maxNumCycles - cyclesRemain + 1) + 
                ", Population ratio: " + popRatio)
            
            if(cyclesRemain > 1 && !popConvergance(popRatio)) 
                inter(newGroup, cyclesRemain - 1)
            else newGroup
        }
        inter(group, maxNumCycles)
    }
    
    def popConvergance(ratio: (Double, Double)): Boolean = {
        ratio._1 == 1.0 || ratio._2 == 1.0
    }
    

}