package rwexample1.nlp.segmentation

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._
import scala.collection.JavaConversions._
import java.util.Properties
import edu.stanford.nlp.util.PropertiesUtils
import edu.stanford.nlp.sequences.SeqClassifierFlags

object ArabicSegmenter extends Controller {

    val options = new Properties()
    val basedir = "data"
    options.setProperty("loadClassifier", basedir + "/arabic-segmenter-atb+bn+arztrain.ser.gz")
    options.setProperty("inputEncoding", "UTF-8")
    
    val segmenter = new edu.stanford.nlp.international.arabic.process.ArabicSegmenter(options)
    segmenter.loadSegmenter(segmenter.getFlags().loadClassifier, options)
    
    // Simple string segmentization
    def segmentizeString = Action { implicit request =>
        request.body.asJson.map { json =>
            val sourceText = (json \ "srcTxt").validate[String]
            val segmentedString = segmenter.segmentString(sourceText.get)
            val segments = segmentedString.split(" ")
            val output = for (segment <- segments ) yield segment
            Ok(Json.toJson(output)) 
        }.getOrElse {
            BadRequest("Expecting Json data")
        }
    }
    
    // Segmentize uploaded file
    def segmentizeFile = Action(parse.multipartFormData) { request =>
        request.body.file("file_ar").map { file =>
            val srcFile = file.ref.file
            val lines = scala.io.Source.fromFile(srcFile)
            val lineList = lines.getLines.toList
            val tokLines = lineList map { line =>
                val segmentedString = segmenter.segmentString(line)
                segmentedString.split(" ")
            }
            Ok(Json.toJson(tokLines))
        }.getOrElse {
            BadRequest("Expecting Json data")
        }
    }
}