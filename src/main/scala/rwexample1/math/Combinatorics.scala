package rwexample1.math

object Combinatorics extends App {
    
    // recursive
    // See p. 90 Schaum Discrete Mathematics
    // Naive implementation. 
	// For large numbers see http://en.wikipedia.org/wiki/Binomial_coefficient#Binomial_coefficient_in_programming_languages
    def binomialCoeficients(n: Int, r: Int): Double = {
        def inner(n: Int, r: Int, n_acc: Int, r_acc: Int): Double = {
	        if (r >= 1) {
	            val new_n_acc = n_acc * n
	            val new_r_acc = r_acc * r
	            inner(n-1, r-1, new_n_acc, new_r_acc)
	        }
	        
	        else {
	            n_acc.toDouble / r_acc.toDouble
	        }
        }
        inner(n, r, 1, 1)
    } // end binomialCoeficients
    
    def numPerms(n: Int)(r: Int*): Int = {
        println("n:" + n + " | r:" + r)
        
        def denom(d: Seq[Int], acc: Int): Int = {
            d match {
                //println("d: " + d + " | acc: " + acc)
                case Nil => acc
                case x :: xs => {
                    denom(xs, factorial(x) * acc) // recurse
                }
            }
        }
        denom(r, 1)
    }
    
    // n * n-1 ... * (n-(n-1))
    def factorial(n: Int): Int = {
        if (n < 0) throw new IllegalArgumentException("Input must be greater than 0")
        val orig = n // diagnostic
        def recurse(n: Int, prod: Int): Int = {
            if (n <= 1) prod // return accumulator
            else {
                println(n + " * " + prod)
                recurse(n-1, n*prod)
            }
        }
        recurse(n, 1)
    }
        
    def permutationSampleSize(setSize: Int, sampleSize: Int, withReplacement: Boolean = false): Int = {
        val replacement = if(withReplacement) 1 else 0
        // recursion
        def inner(setSize: Int, sampleSize: Int, permAcc: Int): Int = {
            if (sampleSize >= 1) {
                inner(setSize - replacement, sampleSize - 1, setSize * permAcc)
            }
            else permAcc
        }
        inner(setSize, sampleSize, 1)
    }
    
    // Permutation sample size examples
    println(permutationSampleSize(52, 3)) // should be 132600
    println(permutationSampleSize(52, 3, true)) // should be 140608
    println(permutationSampleSize(4, 3)) // 24 = 4 * 3 * 2
    println(permutationSampleSize(4, 3, true)) // 64 = 4 * 4 * 4
}