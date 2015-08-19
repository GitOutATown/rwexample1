package rwexample1.nlp.segmentation

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._

import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.ling.HasWord
import edu.stanford.nlp.process.CoreLabelTokenFactory
import edu.stanford.nlp.process.DocumentPreprocessor
import edu.stanford.nlp.process.PTBTokenizer

import java.io.FileReader
import java.io.StringReader

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object EnglishTokenizer {

    // Simple string tokenization
    def tokenizeString = Action { implicit request =>
        request.body.asJson.map { json =>
            val sourceText = (json \ "srcTxt").validate[String]
            
            val tokenizer = new PTBTokenizer(new StringReader(sourceText.get),
                new CoreLabelTokenFactory(), "")
            
            val tokens = new ListBuffer[String]
            while(tokenizer.hasNext()) {
                val token = tokenizer.next().word()
                tokens += token
            }
    
            Ok(Json.toJson(tokens.toList))
        
        }.getOrElse {
            BadRequest("Expecting Json data")
        }
    }
    
    // File upload and tokenize
    def tokenizeFile = Action(parse.multipartFormData) { request =>
        request.body.file("file_en").map { file =>
            val srcFile = file.ref.file
            
            val tokenizer = new PTBTokenizer(new FileReader(srcFile),
                new CoreLabelTokenFactory(), "")
            
            val tokens = new ListBuffer[String]
            while(tokenizer.hasNext()) {
                val token = tokenizer.next().word()
                tokens += token
            }
    
            Ok(Json.toJson(tokens.toList))
            
        }.getOrElse {
            BadRequest("Missing file")
        }
    }
}