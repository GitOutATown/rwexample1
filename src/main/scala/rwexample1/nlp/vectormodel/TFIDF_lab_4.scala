package lab.nlp.vectorspacemodel.tfidf

import common.Path._
import scala.io.Source
import scala.collection.mutable.Map
import java.io.File
import math._

object TFIDF_lab_4 extends App {
    
    // Documents location
    val resource_path = "src/main/scala/resources/"
    val path = root_path + resource_path
    val corpusDir = path + "shakespeare/"
    
    // Used for filtering out unimportant terms
    val stopwords = Source.fromFile(path + "stopwords.txt").getLines.toList
    
    // Load all txt files in corpus directory
    val corpus = new File(corpusDir).listFiles.filter(_.getName.endsWith(".txt"))
    
    // Holds count of number of docs term appears in.
    val docsPerTerm = Map.empty[String, Int]
    
    // Array of term counts per doc.
    val termCountsPerDoc = for(file <- corpus) yield mapTerms(file)
    
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
    
    // Parse file, filter, count terms. Returns tuple of (file name, term counts)
    def mapTerms(file: File) = {
        val doc = Source.fromFile(file)
        val terms = doc.getLines.flatMap(preprocess(_))
            .filter(term => !stopwords.contains(term) && term != "")
            
        // Holds term counts for this doc. Mutable (side effect). Re-implement in Spark for immutability and scalability.
        val termCounts = Map.empty[String, Int] // not parallelizable
        // Count number of times each term occurs in this doc.
        terms.foreach {
            term => termCounts.get(term) match {
                case Some(_) => termCounts(term) += 1
                case None => termCounts(term) = 1
            }
        }
        
        (file.getName, termCounts)
    } // end mapTerms
    
    def preprocess(str: String): Array[String] = {
        val splits = str.split("[ !,.:;]+").map(_.toLowerCase)
        splits.map { s => s.replaceAll("(?m)^[ \t]*\r?\n", "") }
    }
    
    // Display term frequency scores for each doc
    /*tfCounts foreach {
        doc => {
            println(doc._1); // doc name
            println(doc._2.take(20)) // term frequencies
            println
        }
    }*/
    
    // log2 function
    def log2(x: Double) = scala.math.log(x)/scala.math.log(2)
    
    // Calculate TFIDF (culmination)
    // Sort remains by TF to highlight effect of IDF
    val allTFIDFs = tfsPerDoc.map{
        case (title, terms) => {
            val tfidfs = terms.map{
                case (term, tf) => {
                    val idf = log2(docsPerTerm.size.toFloat / docsPerTerm(term))
                    (term, tf * idf)
                }
            }
            (title, tfidfs)
        }
    } // end allTFIDFs
    
    // Display output of all TFIDF scores per doc
    allTFIDFs foreach {
        doc => {
            println(doc._1)
            println(doc._2.take(10))
            println()
        }
    }
}

