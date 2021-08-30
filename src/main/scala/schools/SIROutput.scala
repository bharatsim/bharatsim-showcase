package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.EmptyPattern
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import schools.InfectionStatus._

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
    val graphProvider = context.graphProvider
    val label = "Person"
    val row = List(
      context.getCurrentStep * Disease.dt,
      graphProvider.fetchCount(label, "infectionState" equ Susceptible),
      graphProvider.fetchCount(label, "infectionState" equ Exposed),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic),
      graphProvider.fetchCount(label, "infectionState" equ InfectedMild),
      graphProvider.fetchCount(label, "infectionState" equ InfectedSevere),
      graphProvider.fetchCount(label, "infectionState" equ Recovered),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalised),
      graphProvider.fetchCount(label, "infectionState" equ Dead),
      Main.vaccinesAdministered,
      graphProvider.fetchCount(label, EmptyPattern()) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)))
    )
    List(row)

  }

}
