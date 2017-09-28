package json


import java.io.{File, PrintWriter}

import org.json4s.jackson.{JsonMethods, Serialization}
import org.json4s.{DefaultFormats, Formats, Serializer}

import scala.io.Source

object Json {

  def extract[T](fileName: String, serializer: Serializer[T])(implicit m: Manifest[T]): T = {
    val source = Source.fromFile(s"json/$fileName.json")
    val json = JsonMethods.parse(source.reader())
    implicit val formats: Formats = DefaultFormats + serializer
    json.extract[T]
  }

  def write[T <: AnyRef](value: T, fileName: String, maybeSerializer: Option[Serializer[T]] = None)(implicit m: Manifest[T]): Unit = {
    implicit val formats: Formats = if (maybeSerializer.isDefined) {
      DefaultFormats + maybeSerializer.get
    } else {
      DefaultFormats
    }
    val json = Serialization.writePretty[T](value)
    val pw = new PrintWriter(new File(s"json/$fileName.json"))
    pw.write(json)
    pw.close()
  }

}