package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs

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
    val row = List(
      context.getCurrentStep * Main.dt,
      graphProvider.fetchCount(label, "wasInfectedBy" equ "Asymptomatic"),
      graphProvider.fetchCount(label, "wasInfectedBy" equ "Presymptomatic"),
      graphProvider.fetchCount(label, "wasInfectedBy" equ "InfectedMild"),
      graphProvider.fetchCount(label, "wasInfectedBy" equ "InfectedSevere"),
      graphProvider.fetchCount(label, "wasInfectedBy" equ "Hospitalised"),
      graphProvider.fetchCount(label, "wasInfectedBy" equ "FOI"),
      graphProvider.fetchCount(label, "wasInfectedAt" equ "Home"),
      graphProvider.fetchCount(label, "wasInfectedAt" equ "Office"),
      graphProvider.fetchCount(label, "wasInfectedAt" equ "ClassRoom"),
    )
    List(row)

  }

}
