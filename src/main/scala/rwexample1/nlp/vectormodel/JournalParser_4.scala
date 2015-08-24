package lab.nlp.vectorspacemodel.tfidf.journal

object JournalParser_4 {
    
    val datePattern = """[0-9]{1,2}/[0-9]{1,2}/[0-9]{1,4}""".r
    
    def isDate(line: String): Boolean = {
        datePattern.findPrefixMatchOf(line) match {
            case Some(_) => true
            case None => false 
        }
    }
    
    def process(text: List[String]): List[Document] = {
        
        def inter(interText: List[String], journal: List[Document], entry: Document = null): List[Document] = {
            interText match {
                case Nil => journal :+ entry
                case line :: tail => {
                    if(isDate(line)) { // new entry
                        val title = line
                        val iterJournal = if(entry == null) journal else journal :+ entry
                        inter(tail, iterJournal, Document(title, List[String]()))
                    }
                    else { // continuing as part of existing entry
                        val iterEntry = {
                            // Catch potential null entry scenarios, such as first line in doc not being date or bad date formating.
                            if(entry != null) Document(entry.title, entry.text :+ line)
                            else Document("DATE MISSING", List(line)) // Creating new entry
                        }
                        inter(tail, journal, iterEntry)
                    }
                }
            }
        } // End inter
        
        inter(text, List[Document]())
    } // End process
}



