package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.listeners.CSVSpecs
import schools.InfectionStatus._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class WardwiseOutputSpec(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] = List("Day", "Ward", "InfectedCount", "RecoveredCount")

  override def getRows(): List[List[Any]] = {
    if (context.getCurrentStep % (5 * Disease.inverse_dt) == 0) {  // get data only every 5 days
      val infectedStates = List(Exposed.toString, Asymptomatic.toString, Presymptomatic.toString, InfectedMild.toString, InfectedSevere.toString, Hospitalised.toString)
      val label = "Person"
      val countByWard = new mutable.HashMap[String, (Int, Int)]()  // values are (infectedCount, recoveredCount)

      val people = context.graphProvider.fetchNodes(label)

      people.foreach(p => {
        val ward = p.getParams.apply("villageTown").toString
        val infectionState = p.getParams.apply("infectionState").toString
        if (infectedStates.contains(infectionState)) {
          val currentCounts = countByWard.getOrElseUpdate(ward, (0, 0))
          countByWard.put(ward, (currentCounts._1 + 1, currentCounts._2))
        }
        else if (infectionState == Recovered.toString) {
          val currentCounts = countByWard.getOrElseUpdate(ward, (0, 0))
          countByWard.put(ward, (currentCounts._1, currentCounts._2 + 1))
        }
      })

      val rows = ListBuffer.empty[List[String]]
      countByWard.toList.foreach((kv) => {
        val ward = kv._1
        val counts = kv._2
        rows.addOne(List((context.getCurrentStep/Disease.inverse_dt).toString, ward, counts._1.toString, counts._2.toString))
      })
      return rows.toList
    }
    else {
      List.empty
    }
  }
}
