package rwexample1.nlp.segmentation

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json._
import scala.collection.JavaConversions._
import java.util.Properties;
import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ling.CoreLabel

object ChineseSegmenter extends Controller {
	
	val basedir = System.getProperty("ChineseSegmenter", "data")
	println("basedir: " + basedir)
	val props = new Properties()
	props.setProperty("sighanCorporaDict", basedir)
	props.setProperty("serDictionary", basedir + "/dict-chris6.ser.gz")
	props.setProperty("inputEncoding", "UTF-8")
    props.setProperty("sighanPostProcessing", "true")
    val segmenter = new CRFClassifier[CoreLabel](props)
    segmenter.loadClassifierNoExceptions(basedir + "/ctb.gz", props)
  
	// Simple string segmentization
	def segmentizeString = Action { implicit request =>
	  	request.body.asJson.map { json =>
	  	  	val sourceText = (json \ "srcTxt").validate[String]
	  	  	val segmented = segmenter.segmentString(sourceText.get)
	  	  	val output = for (segment <- segmented ) yield segment
	  	  	Ok(Json.toJson(output))
	  	}.getOrElse {
			BadRequest("Expecting Json data")
		}
	}
	
	// File upload segmentization
	def segmentizeFile = Action(parse.multipartFormData) { request =>
		request.body.file("file_ch").map { file =>
			val srcFile = file.ref.file
            val lines = scala.io.Source.fromFile(srcFile)
			val lineList = lines.getLines.toList
						
			val tokLines = lineList map { line =>
			   	val tokens = segmenter.segmentString(line)
			   	val output = for {
			   		token <- tokens
			   	} yield Json.toJson(token)
			   	output
			}
			
			Ok(Json.toJson(tokLines))
		}.getOrElse {
			BadRequest("Missing file")
		}
	}
}