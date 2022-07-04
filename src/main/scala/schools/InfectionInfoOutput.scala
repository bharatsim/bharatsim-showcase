package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import schools.InfectionStatus._

import scala.collection.mutable

class InfectionInfoOutput(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] =
    List(
      "#Day",
      "InfectedByAsymptomatic",
      "InfectedByPresymptomatic",
      "InfectedByInfectedMild",
      "InfectedByInfectedSevere",
      "InfectedByHospitalised",
      "InfectedByFOI",
      "InfectedAtHome",
      "InfectedAtOffice",
      "InfectedAtClassRoom"
    )

  override def getRows(): List[List[Any]] = {
    val graphProvider = context.graphProvider
    val label = "Person"
    val infectedByMap = mutable.HashMap.empty[String, Int]
    val infectedAtMap = mutable.HashMap.empty[String, Int]
    val nodes = graphProvider.fetchNodes(label)
    nodes.foreach(node => {
      val infectedBy = node.getParams.apply("wasInfectedBy").toString
      val existingInfectedByCount = infectedByMap.getOrElse(infectedBy, 0)
      infectedByMap.put(infectedBy, existingInfectedByCount + 1)
      val infectedAt = node.getParams.apply("wasInfectedAt").toString
      val existingInfectedAtCount = infectedAtMap.getOrElse(infectedAt, 0)
      infectedAtMap.put(infectedAt, existingInfectedAtCount + 1)
    })

    val row = List(
      context.getCurrentStep,
      infectedByMap.getOrElse(Asymptomatic.toString, 0),
      infectedByMap.getOrElse(Presymptomatic.toString, 0),
      infectedByMap.getOrElse(InfectedMild.toString, 0),
      infectedByMap.getOrElse(InfectedSevere.toString, 0),
      infectedByMap.getOrElse(Hospitalised.toString, 0),
      infectedByMap.getOrElse("FOI", 0),
      infectedAtMap.getOrElse("Home", 0),
      infectedAtMap.getOrElse("Office", 0),
      infectedAtMap.getOrElse("ClassRoom", 0)
    )
    return List(row)

  }

}
