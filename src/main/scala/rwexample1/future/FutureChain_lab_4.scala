package courses.coursera.reactive_programming.week3

import scala.concurrent._
import ExecutionContext.Implicits.global

object FutureChain_lab_4 extends App {
    
    // http://docs.scala-lang.org/overviews/core/futures.html
    /* >>>
     * One of the design goals for futures was to enable their use in 
     * for-comprehensions. For this reason, futures also have the flatMap, 
     * map, filter, and foreach combinators. The flatMap method takes a 
     * function that maps the value to a new future g, and then returns a 
     * future which is completed once g is completed.
     * <<< */
    
    // Business logic
    import courses.coursera.reactive_programming.week3.TradingServices._

    val purchase: Future[Int] = for {
        rateQuote <- Future[Double] { getCurrentValue } // flatMap
        profitable <- Future[Boolean] { // map
            if(isProfitable(rateQuote)) true // dependency on rateQuote
            else throw new Exception("not profitable")
        } recover { // Example of recover in for-comprehension
            case e: ConnectionException => {
                // Recursive for set number of attempts
                if(recoverConnection(rateQuote, 3)) true
                else throw e
            }
            // TODO: Other exception cases
        }
    } yield (buy(amount, rateQuote)) // yield is part of the map future propagation
    
    purchase onSuccess {
        case _ => println("~~Purchased " + amount + " USD")
    }
    
    purchase onFailure {
        case e => println("~~Exception: " + e)
    }
    
    // Recursive with number of attempts
    def recoverConnection(rateQuote: Double, attempts: Int): Boolean = {
        println("In recoverConnection attempt " + attempts)
        try {
            isProfitable(rateQuote)
        } catch {
            case e: ConnectionException =>
                if(attempts > 0) recoverConnection(rateQuote, attempts - 1) 
                else false
        }                    
    }
    
    // ------------------------ //
    
    val amount = 50
    
    println("TCB")
    Thread.sleep(2500) // keep jvm running long enough for futures to complete
    println("JVM leaving the house.")
}