package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import schools.InfectionStatus._

import scala.collection.mutable

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
    val countMap = mutable.HashMap.empty[String, Int]
    val nodes = graphProvider.fetchNodes(label)
    nodes.foreach(node => {
      val infectedState = node.getParams.apply("infectionState").toString
      val ageCateg = map_ages_to_categs(node.getParams.apply("age").toString.toInt)
      val existingCount = countMap.getOrElse(infectedState + ageCateg, 0)
      countMap.put(infectedState + ageCateg, existingCount + 1)

    })

    val row = List(
      context.getCurrentStep * Main.dt,
      countMap.getOrElse(Susceptible.toString + "0-18", 0),
      countMap.getOrElse(Exposed.toString + "0-18", 0),
      countMap.getOrElse(Asymptomatic.toString + "0-18", 0),
      countMap.getOrElse(Presymptomatic.toString + "0-18", 0),
      countMap.getOrElse(InfectedMild.toString + "0-18", 0),
      countMap.getOrElse(InfectedSevere.toString + "0-18", 0),
      countMap.getOrElse(Recovered.toString + "0-18", 0),
      countMap.getOrElse(Hospitalised.toString + "0-18", 0),
      countMap.getOrElse(Dead.toString + "0-18", 0),
      Main.ageWiseVaccinesAdministered(0) + Main.ageWiseVaccinesAdministered(1),
      graphProvider.fetchCount(label, ("age" gte 0) and ("age" lt 18)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 0) and ("age" lt 18)),

      countMap.getOrElse(Susceptible.toString + "18-45", 0),
      countMap.getOrElse(Exposed.toString + "18-45", 0),
      countMap.getOrElse(Asymptomatic.toString + "18-45", 0),
      countMap.getOrElse(Presymptomatic.toString + "18-45", 0),
      countMap.getOrElse(InfectedMild.toString + "18-45", 0),
      countMap.getOrElse(InfectedSevere.toString + "18-45", 0),
      countMap.getOrElse(Recovered.toString + "18-45", 0),
      countMap.getOrElse(Hospitalised.toString + "18-45", 0),
      countMap.getOrElse(Dead.toString + "18-45", 0),
      Main.ageWiseVaccinesAdministered(2) + Main.ageWiseVaccinesAdministered(3),
      graphProvider.fetchCount(label, ("age" gte 18) and ("age" lt 45)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 18) and ("age" lt 45)),

      countMap.getOrElse(Susceptible.toString + "45-60", 0),
      countMap.getOrElse(Exposed.toString + "45-60", 0),
      countMap.getOrElse(Asymptomatic.toString + "45-60", 0),
      countMap.getOrElse(Presymptomatic.toString + "45-60", 0),
      countMap.getOrElse(InfectedMild.toString + "45-60", 0),
      countMap.getOrElse(InfectedSevere.toString + "45-60", 0),
      countMap.getOrElse(Recovered.toString + "45-60", 0),
      countMap.getOrElse(Hospitalised.toString + "45-60", 0),
      countMap.getOrElse(Dead.toString + "45-60", 0),
      Main.ageWiseVaccinesAdministered(4) + Main.ageWiseVaccinesAdministered(5),
      graphProvider.fetchCount(label, ("age" gte 45) and ("age" lt 60)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 45) and ("age" lt 60)),

      countMap.getOrElse(Susceptible.toString + "60+", 0),
      countMap.getOrElse(Exposed.toString + "60+", 0),
      countMap.getOrElse(Asymptomatic.toString + "60+", 0),
      countMap.getOrElse(Presymptomatic.toString + "60+", 0),
      countMap.getOrElse(InfectedMild.toString + "60+", 0),
      countMap.getOrElse(InfectedSevere.toString + "60+", 0),
      countMap.getOrElse(Recovered.toString + "60+", 0),
      countMap.getOrElse(Hospitalised.toString + "60+", 0),
      countMap.getOrElse(Dead.toString + "60+", 0),
      Main.ageWiseVaccinesAdministered(6) + Main.ageWiseVaccinesAdministered(7) + Main.ageWiseVaccinesAdministered(8) + Main.ageWiseVaccinesAdministered(9),
      graphProvider.fetchCount(label, ("age" gte 60)) - graphProvider.fetchCount(label, (("vaccinationStatus" equ false) and ("infectionState" equ Susceptible)) and ("age" gte 60))
    )
    List(row)

  }

  def map_ages_to_categs(age: Int): String = {
    if (0 <= age && age < 18) {"0-18"}
    else if (18 <= age && age < 45) {"18-45"}
    else if (45 <= age && age < 60) {"45-60"}
    else {"60+"}
  }

}
