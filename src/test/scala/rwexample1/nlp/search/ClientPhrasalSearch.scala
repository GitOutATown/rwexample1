package rwexample1.nlp.search

import rwexample1.nlp.search.PhrasalSearch._
import util.math.Rounding_lab_1._

import scala.collection.mutable.Map
import scala.collection.immutable.Map
import scala.collection.mutable.SortedSet
import scala.math.log10
 
/**
 * This is an exercise in phrasal search following Manning's Intro to IR, Ch. 2
 * Not using POS or stop words.
 * Doing lowercasing of index and query.
 * Using Term Frequency for ranking. Not using IDF because collection is too small.
 * Some open questions:
 * * Whether to use a sliding window strategy (on the phrase query)
 * * Basis for phrase retrieval ranking
 * * Whether to use a proximity parameter on biword pairs (affects ranking)
 * * How to account for repeat terms in query
 */
object PhrasalSearch_lab_12 extends App {
      // Document path
    val root_path = "Intro IR Manning/"
    val doc_path = "src/main/resources/shakespeare/"
    
    val docList = List[Document](
        Document(IDGenerator.next, "Antony_and_Cleopatra.txt", "William Shakespear"),
         Document(IDGenerator.next, "Hamlet.txt", "William Shakespear"),
         Document(IDGenerator.next, "Julius_Caesar.txt", "William Shakespear"),
         Document(IDGenerator.next, "Macbeth.txt", "William Shakespear"),
         Document(IDGenerator.next, "Othello.txt", "William Shakespear"),
         Document(IDGenerator.next, "The_Tempest.txt", "William Shakespear")
    )
  
    // Put docs in Catalog (i.e. Map)
    docList.foreach(doc => docCatalog += (doc.id -> doc))
  
    // ------ Indexing ------------------- //
  
    // partial setup with doc paths
    val index = indexDocument(root_path + doc_path)_
  
    // Initiate indexing
    docList foreach index
 
      // ------ Logging indexing results ------ //
    
    println("Dictionary terms size: " + dictionary.keys.size)
    
    docList foreach(doc => {
        println("------------------")
        println(doc.name)
        println("Unique terms: " + doc.tokensUnique.size)
        println("Word count: " + doc.tokensTotal)
    })
  
    // ----- Phrasal query examples ------------//

    val queryFrequency = freqRank(5)_ // set rank rounding
    //val queryResults = queryFrequency("Look, where they come")
    //val queryResults = queryFrequency("Take but good note")
    //val queryResults = queryFrequency("pillar of the world")
    val queryResults = queryFrequency("our dungy earth alike")
    //val queryResults = queryFrequency("earth alike")
    //val queryResults = queryFrequency("And, in conclusion, Elvis")
    
    queryResults foreach(
        queryResults => {
            println("----------------------------------")
            queryResults.values.foreach(insts => {
                println(insts.locs.docId + ", " + insts.locs.term + ", " + insts.rankMetric)
            })
        }
    )
  
    val biWordResults = biWord(queryResults)
    biWordResults foreach println
}



