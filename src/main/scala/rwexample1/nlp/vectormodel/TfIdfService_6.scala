package lab.nlp.vectorspacemodel.tfidf.journal

import common.Path._
import scala.io.Source
import java.io.File
import math._
import util.MathUtil._

/*
 * I am, somewhat paradocically, using Maps instead of Matrices as my 
 * mechanism for tallying, counting, and looking up frequency values.
 * This is an early stage convience for developing my intuition for 
 * Semantic Vector Models.
 */
import scala.collection.mutable.Map

object TfIdfService_6 {
        
    // Holds count of number of docs each term appears in.
    val docsPerTerm = Map.empty[String, Int]
    
    // ----- Methods ----------------- //
    
    def tfidf(corpus: List[Document], stopwords: List[String])
        :List[(String, List[(String, Double)])] = {
        
        // Array of term counts per doc.
        val termCountsPerDoc = for(file <- corpus) yield mapTerms(file, stopwords)
    
        // Calculate term frequency for each term in each doc
        val tfsPerDoc = termCountsPerDoc.map{
            case (title, termCounts) => {
                
                // Tally number of docs each term appears in (side effect)
                termCounts.foreach{
                    case (term, _) => docsPerTerm.get(term) match {
                        case Some(_) => docsPerTerm(term) += 1
                        case None => docsPerTerm(term) = 1
                    }
                }
            
                // counts, frequency ratio (normalized)
                val maxFreq = (termCounts.values.max).toFloat
                val freqs = termCounts.map{
                    case (term, count) => (term, count / maxFreq)
                }
                val freqsList = freqs.toList.sortBy(_._2).reverse
                (title, freqsList)
            } // end case
        } // END termDocMaps.map
        
        // Calculate TFIDF (culmination)
        val rnd = roundAt(3)_
        val allTFIDFs = tfsPerDoc.map{
            case (title, terms) => {
                val tfidfs = terms.map{
                    case (term, tf) => {
                        val idf = log2(docsPerTerm.size.toFloat / docsPerTerm(term))
                        (term, rnd(tf * idf))
                    }
                }
                (title, tfidfs.sortBy(_._2).reverse)
            }
        } // end allTFIDFs
        
        allTFIDFs // return result
    } // END tdidf
    
        // Parse file, filter, count terms. Returns tuple of (file name, term counts)
    def mapTerms(file: Document, stopwords: List[String]) = {
        
        def filterTerm(term: String): Boolean = {
            if(!stopwords.contains(term) && term != "") true
            else {
                //println("===>filtering out: " + term)
                false
            }
        }
        
        //val doc = Source.fromFile(file)
        val terms = file.text.flatMap(preprocess(_))
            .filter(term => filterTerm(term))
            
        //println("terms after filtering: " + terms)
            
        // Holds term counts for this doc. Mutable (side effect). Re-implement in Spark for immutability and scalability.
        val termCounts = Map.empty[String, Int] // not parallelizable
        // Count number of times each term occurs in this doc.
        terms.foreach {
            term => termCounts.get(term) match {
                case Some(_) => termCounts(term) += 1
                case None => termCounts(term) = 1
            }
        }
        
        (file.title, termCounts)
    } // end mapTerms
    
    def preprocess(str: String): Array[String] = {
        val splits = str.split("""[ !?,.:;()"]+""").map(_.toLowerCase)
        splits.map { s => s.replaceAll("(?m)^[ \t]*\r?\n", "") }
    }
    
    // log2 function
    def log2(x: Double) = scala.math.log(x)/scala.math.log(2)
}

