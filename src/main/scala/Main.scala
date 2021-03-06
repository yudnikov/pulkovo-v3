import json.{Json, VectorSerializer}

object Main extends App {

  type Row = Vector[Option[String]]
  type Table = List[Row]

  val fragments: Table = Json.extract[List[Row]]("input", VectorSerializer)

  def legsAggregations[T](fragments: Table)(job: (Array[String]) => T): List[T] = {

    def isValidPair(a: Row, b: Row): Boolean = {
      a.zip(b).forall {
        t => t._1.isEmpty | t._2.isEmpty
      }
    }

    def join(a: Row, b: Row): Row = {
      a zip b map { ab =>
        if (ab._1.isDefined) ab._1 else ab._2
      }
    }

    def combinations(fs: Table, agr: Row = Vector.empty): Table = {
      if (agr.isEmpty) {
        val res = for {
          f <- fs
          if f.head.isDefined
        } yield
          combinations(fs.filter(_.head.isEmpty), f)
        fs.filter(_.forall(_.isDefined)).union(res.flatten.distinct)
      } else {
        val joins = fs.collect {
          case row if isValidPair(agr, row) => join(agr, row)
        }
        val result = {
          for {
            j <- joins
            if j.exists(_.isEmpty)
          } yield
            combinations(fs.union(joins), j)
        }.flatten
        joins.filter(_.forall(_.isDefined)) union result
      }
    }

    val result = combinations(fragments)

    //json
    Json.write(result, "output")

    result.map(_.map(_.get).toArray).map(job)

  }

  val result = legsAggregations(fragments) { legs =>
    // ... havy calculation...
    var data = legs.mkString("-");
    // ... havy calculation...
    data
  }

  //Json.write(result, "output")
  //println(result.mkString("\n"))

}
