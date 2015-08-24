package lab.nlp.vectorspacemodel.tfidf.journal

import scala.io.Source
import lab.nlp.vectorspacemodel.tfidf.journal.JournalParser_4._
import lab.nlp.vectorspacemodel.tfidf.journal.TfIdfService_6._
import common.Path._

object TfidfClient_journal extends App {

    val resource_path = "src/main/scala/resources/"
    val path = root_path + resource_path
    val docName = "rw journal - sv 3.txt"
    
    val doc = Source.fromFile(journalPath + docName)
    val text = doc.getLines.toList
    doc.close // should be in finally
    
    val stopwords = Source.fromFile(path + "stopwords2.txt").getLines.toList
        .filter(w => w != "")
    
    val journalEntries = process(text)
    
    println("Number of journal entries: " + journalEntries.length + "\n")
    
    val allTFIDFs = tfidf(journalEntries, stopwords)
    
    println("TFIDF weights for each entry:\n")
    allTFIDFs foreach {
        doc => {
            println(doc._1)
            println(doc._2.take(10))
            println()
        }
    }
    
    

    
    
    
    
}