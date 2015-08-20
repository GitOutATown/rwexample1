package courses.langmead.tries

object TrieLab extends App {
    
    // Suffix trie
    // https://www.youtube.com/watch?v=hLsrPsFHPcQ
    
    val T = "abaaba"
    val T$ = T + "$"
    println("T$: " + T$ + "\n")
    
    val root = new Node('R')
    
    for{ // Take each suffix of a String and build a trie
        i <- 0 to T$.length() - 1
    } yield root.place(T$.substring(i).toList)
    
    case class Node(val nodeData: Char, var children: List[Node] = Nil) {
        def place(suffix: List[Char]) {
            suffix match {
                case Nil => // on to the next suffix
                case head :: tail => contains(children, head) match {
                    case Some(charNode) => charNode.place(tail)
                    case None => 
                        val charNode = Node(head)
                        children = charNode :: children
                        charNode.place(tail)
                }
            }
        } // end def place
        
        def contains(children: List[Node], ch: Char): Option[Node] = {
            (for {
                node <- children
                if (node.nodeData == ch)
            } yield node).headOption
        }
        
    } // end case class Node
    
    println("trie: " + root)
    
}






