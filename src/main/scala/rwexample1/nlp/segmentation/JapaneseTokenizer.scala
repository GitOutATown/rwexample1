package rwexample1.nlp.segmentation

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._

import com.atilika.kuromoji.AbstractTokenizer;
import com.atilika.kuromoji.Token;
//import com.atilika.kuromoji.AbstractTokenizer.Mode;

import scala.collection.JavaConversions._

object JapaneseTokenizer extends Controller {
  
    // Simple string tokenize
    def tokenizeString = Action { implicit request =>
          request.body.asJson.map { json =>
              val sourceText = (json \ "srcTxt").validate[String]
              val tokenizer = AbstractTokenizer.builder().build()
              val tokens = tokenizer.tokenize(sourceText.get)
              
              val output = for {
                  token <- tokens
              } yield Json.obj("token" -> token.getSurfaceForm(), "features" -> token.getAllFeatures())
              
              Ok(Json.toJson(output))
        }.getOrElse {
            BadRequest("Expecting Json data")
        }
    }
    
    // File upload and tokenize
    def tokenizeFile = Action(parse.multipartFormData) { request =>
        request.body.file("file_ja").map { file =>
            val srcFile = file.ref.file
            val lines = scala.io.Source.fromFile(srcFile)
            val lineList = lines.getLines.toList
            val tokenizer = AbstractTokenizer.builder().build()
            
            val tokLines = lineList map { line =>
                val tokens = tokenizer.tokenize(line)
                val output = for {
                    token <- tokens
                } yield Json.obj("token" -> token.getSurfaceForm(), "features" -> token.getAllFeatures())
                output
            }
            
            Ok(Json.toJson(tokLines))
        }.getOrElse {
            BadRequest("Missing file")
        }
    }

}