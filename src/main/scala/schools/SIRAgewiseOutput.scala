package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import schools.InfectionStatus._

class SIRAgewiseOutput(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] =
    List(
      "#Day",
      "Susceptible - Low Risk (0-18)",
      "Exposed - Low Risk (0-18)",
      "Asymptomatic - Low Risk (0-18)",
      "Presymptomatic - Low Risk (0-18)",
      "InfectedMild - Low Risk (0-18)",
      "InfectedSevere - Low Risk (0-18)",
      "Recovered - Low Risk (0-18)",
      "Hospitalised - Low Risk (0-18)",
      "Dead - Low Risk (0-18)",
      "Vaccinated - Low Risk (0-18)",
      "Background Seropositivity - Low Risk (0-18)",

      "Susceptible - Mid Risk (18-45)",
      "Exposed - Mid Risk (18-45)",
      "Asymptomatic - Mid Risk (18-45)",
      "Presymptomatic - Mid Risk (18-45)",
      "InfectedMild - Mid Risk (18-45)",
      "InfectedSevere - Mid Risk (18-45)",
      "Recovered - Mid Risk (18-45)",
      "Hospitalised - Mid Risk (18-45)",
      "Dead - Mid Risk (18-45)",
      "Vaccinated - Mid Risk (18-45)",
      "Background Seropositivity - Mid Risk (18-45)",

      "Susceptible - Mid Risk (45-60)",
      "Exposed - Mid Risk (45-60)",
      "Asymptomatic - Mid Risk (45-60)",
      "Presymptomatic - Mid Risk (45-60)",
      "InfectedMild - Mid Risk (45-60)",
      "InfectedSevere - Mid Risk (45-60)",
      "Recovered - Mid Risk (45-60)",
      "Hospitalised - Mid Risk (45-60)",
      "Dead - Mid Risk (45-60)",
      "Vaccinated - Mid Risk (45-60)",
      "Background Seropositivity - Mid Risk (45-60)",

      "Susceptible - High Risk (60+)",
      "Exposed - High Risk (60+)",
      "Asymptomatic - High Risk (60+)",
      "Presymptomatic - High Risk (60+)",
      "InfectedMild - High Risk (60+)",
      "InfectedSevere - High Risk (60+)",
      "Recovered - High Risk (60+)",
      "Hospitalised - High Risk (60+)",
      "Dead - High Risk (60+)",
      "Vaccinated - High Risk (60+)",
      "Background Seropositivity - High Risk (60+)"
    )

  override def getRows(): List[List[Any]] = {
    val graphProvider = context.graphProvider
    val label = "Person"
    val row = List(
      context.getCurrentStep * Disease.dt,
      graphProvider.fetchCount(label, "infectionState" equ Susceptible and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ Exposed and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedMild and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedSevere and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ Recovered and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalised and ("age" gte 0) and ("age" lt 18)),
      graphProvider.fetchCount(label, "infectionState" equ Dead and ("age" gte 0) and ("age" lt 18)),
      Main.ageWiseVaccinesAdministered(0) + Main.ageWiseVaccinesAdministered(1),
      graphProvider.fetchCount(label, ("age" gte 0) and ("age" lt 18)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 0) and ("age" lt 18)),

      graphProvider.fetchCount(label, "infectionState" equ Susceptible and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ Exposed and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedMild and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedSevere and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ Recovered and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalised and ("age" gte 18) and ("age" lt 45)),
      graphProvider.fetchCount(label, "infectionState" equ Dead and ("age" gte 18) and ("age" lt 45)),
      Main.ageWiseVaccinesAdministered(2) + Main.ageWiseVaccinesAdministered(3),
      graphProvider.fetchCount(label, ("age" gte 18) and ("age" lt 45)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 18) and ("age" lt 45)),

      graphProvider.fetchCount(label, "infectionState" equ Susceptible and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ Exposed and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedMild and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedSevere and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ Recovered and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalised and ("age" gte 45) and ("age" lt 60)),
      graphProvider.fetchCount(label, "infectionState" equ Dead and ("age" gte 45) and ("age" lt 60)),
      Main.ageWiseVaccinesAdministered(4) + Main.ageWiseVaccinesAdministered(5),
      graphProvider.fetchCount(label, ("age" gte 45) and ("age" lt 60)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 45) and ("age" lt 60)),

      graphProvider.fetchCount(label, "infectionState" equ Susceptible and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ Exposed and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedMild and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ InfectedSevere and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ Recovered and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalised and ("age" gte 60)),
      graphProvider.fetchCount(label, "infectionState" equ Dead and ("age" gte 60)),
      Main.ageWiseVaccinesAdministered(6) + Main.ageWiseVaccinesAdministered(7) + Main.ageWiseVaccinesAdministered(8) + Main.ageWiseVaccinesAdministered(9),
      graphProvider.fetchCount(label, ("age" gte 60)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 60))
    )
    List(row)

  }

}
