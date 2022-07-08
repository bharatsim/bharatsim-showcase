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

  override def getHeaders: List[String] = List("Step", "latitude", "longitude", "infectedCount")

  private def roundLatLong(lat: String, long: String): (Double, Double) = {
    val scale = 1
    (
      BigDecimal(lat).setScale(scale, BigDecimal.RoundingMode.DOWN).toDouble,
      BigDecimal(long).setScale(scale, BigDecimal.RoundingMode.DOWN).toDouble
    )
  }
  override def getRows(): List[List[Any]] = {
    if (context.getCurrentStep % (5 * Disease.inverse_dt) == 0) {  // get data only every 5 days
      val label = "Person"
      val countByLatLong = new mutable.HashMap[(Double, Double), Int]()

      val people = context.graphProvider.fetchNodes(
        label,
        ("infectionState" equ Asymptomatic) or ("infectionState" equ Presymptomatic) or ("infectionState" equ InfectedMild) or ("infectionState" equ InfectedSevere) or ("infectionState" equ Hospitalised)
      )

      people.foreach((p) => {
        val person = p.as[Person]
        val latLong = roundLatLong(person.lat, person.long)
        val infectedCount = countByLatLong.getOrElseUpdate(latLong, 0)
        countByLatLong.put(latLong, infectedCount + 1)
      })

      val rows = ListBuffer.empty[List[String]]
      countByLatLong.toList.foreach((kv) => {
        val latLong = kv._1
        val count = kv._2
        rows.addOne(List(context.getCurrentStep.toString, latLong._1.toString, latLong._2.toString, count.toString))
      })
      return rows.toList
    }
    else {
      List.empty
    }
  }
}
