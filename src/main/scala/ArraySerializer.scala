import org.json4s.JsonAST.{JArray, JNull, JString}
import org.json4s.{CustomSerializer, JValue}

object ArraySerializer extends CustomSerializer[List[Array[Option[String]]]](_ => ( {
  case JArray(rows: List[JArray]) => {
    rows map {
      case JArray(columns: List[JValue]) => {
        columns map {
          case JString(s) =>
            Some(s)
          case _ =>
            None
        }
      }.toArray
      case _ =>
        Array[Option[String]]()
    }
  }
  case _ =>
    Nil
}, {
  case rows: List[Array[Option[String]]] => JArray(
    rows map {
      row: Array[Option[String]] =>
        JArray(
          {
            row map {
              case Some(str) => JString(str)
              case _ => JNull
            }
          }.toList
        )
    }
  )
}))