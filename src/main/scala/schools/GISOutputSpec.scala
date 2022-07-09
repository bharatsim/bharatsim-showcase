package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import schools.InfectionStatus._
import schools.models.Person

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GISOutputSpec(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] = List("Day", "Latitude", "Longitude", "InfectedCount", "RecoveredCount")

  private def roundLatLong(lat: String, long: String): (Double, Double) = {
    val scale = 3  // number of digits after decimal point
    (
      BigDecimal(lat).setScale(scale, BigDecimal.RoundingMode.DOWN).toDouble,
      BigDecimal(long).setScale(scale, BigDecimal.RoundingMode.DOWN).toDouble
    )
  }
  override def getRows(): List[List[Any]] = {
    if (context.getCurrentStep % (5 * Disease.inverse_dt) == 0) {  // get data only every 5 days
      val label = "Person"
      val infectedStates = List(Exposed.toString, Asymptomatic.toString, Presymptomatic.toString, InfectedMild.toString, InfectedSevere.toString, Hospitalised.toString)

      val countByLatLong = new mutable.HashMap[(Double, Double), (Int, Int)]()

      val people = context.graphProvider.fetchNodes(label)

      people.foreach((p) => {
        val infectionState = p.getParams.apply("infectionState").toString
        if (infectedStates.contains(infectionState)) {
          val latLong = roundLatLong(p.getParams.apply("lat").toString, p.getParams.apply("long").toString)
          val currentCounts = countByLatLong.getOrElseUpdate(latLong, (0, 0))
          countByLatLong.put(latLong, (currentCounts._1 + 1, currentCounts._2))
        }
        else if (infectionState == Recovered.toString) {
          val latLong = roundLatLong(p.getParams.apply("lat").toString, p.getParams.apply("long").toString)
          val currentCounts = countByLatLong.getOrElseUpdate(latLong, (0, 0))
          countByLatLong.put(latLong, (currentCounts._1, currentCounts._2 + 1))
        }
      })

      val rows = ListBuffer.empty[List[String]]
      countByLatLong.toList.foreach((kv) => {
        val latLong = kv._1
        val count = kv._2
        rows.addOne(List((context.getCurrentStep/Disease.inverse_dt).toString, latLong._1.toString, latLong._2.toString, count._1.toString, count._2.toString))
      })
      return rows.toList
    }
    else {
      List.empty
    }
  }
}
