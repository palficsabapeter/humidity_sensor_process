package csaba.palfi

import model.SensorStatistics

class ResultsPrinter(results: Seq[Map[String, SensorStatistics]]) {
  def printResults(): Unit = {
    val processed = processResults
    printResult(processed)
  }

  private def processResults = {
    val aggregated = aggregateResults
    sortResult(aggregated)
  }

  private def aggregateResults =
    (for {
      m  <- results
      kv <- m
    } yield kv).foldLeft(Map.empty[String, SensorStatistics]) { (acc, kv) =>
      val res =
        if (acc.contains(kv._1))
          kv._1 -> (acc(kv._1) + kv._2)
        else
          kv
      acc + res
    }

  private def sortResult(
      result: Map[String, SensorStatistics],
  ): List[SensorStatistics] = {
    result.toList
      .sortWith((r1, r2) =>
        (r1._2.getAvgOpt, r2._2.getAvgOpt) match {
          case (None, None)         => true
          case (Some(_), None)      => true
          case (None, Some(_))      => false
          case (Some(v1), Some(v2)) => v1 > v2
        },
      ).map(a => a._2)
  }

  private def printResult(result: List[SensorStatistics]) = {
    val measurements = result.foldLeft(0)((acc, res) => acc + res.count)
    println(s"Num of processed measurements: $measurements")

    val nanCount = result.foldLeft(0)((acc, res) => acc + res.nanCount)
    println(s"Num of failed measurements: $nanCount\n")
    println("Sensors with highest average humidity:\n")
    println("sensod-id,min,avg,max")
    result.foreach { res =>
      val name = res.name
      val min  = res.min.value.getOrElse("NaN")
      val max  = res.max.value.getOrElse("NaN")
      val avg  = res.getAvgOpt.getOrElse("NaN")
      println(s"$name,$min,$avg,$max")
    }
  }
}
