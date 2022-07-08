package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.EmptyPattern
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import schools.InfectionStatus._

import scala.collection.mutable

class SIROutput(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] =
    List(
      "#Day",
      "Susceptible",
      "Exposed",
      "Asymptomatic",
      "Presymptomatic",
      "InfectedMild",
      "InfectedSevere",
      "Recovered",
      "Hospitalised",
      "Dead",
      "VaccinesAdministered",
      "BackgroundSeropositivity"
    )

  override def getRows(): List[List[Any]] = {
    if (context.getCurrentStep % Disease.inverse_dt == 0) {
      val graphProvider = context.graphProvider
      val label = "Person"

      val countMap = mutable.HashMap.empty[String, Int]
      val nodes = graphProvider.fetchNodes(label)
      nodes.foreach(node => {
        val infectedState = node.getParams.apply("infectionState").toString
        val existingCount = countMap.getOrElse(infectedState, 0)
        countMap.put(infectedState, existingCount + 1)
      })

      val row = List(
        context.getCurrentStep * Disease.dt,
        countMap.getOrElse(Susceptible.toString, 0),
        countMap.getOrElse(Exposed.toString, 0),
        countMap.getOrElse(Asymptomatic.toString, 0),
        countMap.getOrElse(Presymptomatic.toString, 0),
        countMap.getOrElse(InfectedMild.toString, 0),
        countMap.getOrElse(InfectedSevere.toString, 0),
        countMap.getOrElse(Recovered.toString, 0),
        countMap.getOrElse(Hospitalised.toString, 0),
        countMap.getOrElse(Dead.toString, 0),
        Main.vaccinesAdministered,
        graphProvider.fetchCount(label, EmptyPattern()) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)))
      )
      List(row)
    }
    else {
      List.empty
    }
  }

}
