package multipleStrains

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.EmptyPattern
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import multipleStrains.InfectionStatus._

import scala.collection.mutable

class SIROutput(context: Context) extends CSVSpecs {

override def getHeaders: List[String] =
  List(
    "Day",
    "Susceptible",
    "Exposed",
    "Asymptomatic",
    "PreSymptomatic",
    "InfectedMild",
    "InfectedSevere",
    "Hospitalised",
    "Recovered",
    "Deceased",
    "TotalInfected",
    "TotalRemoved",
    "",
    "",
    "Susceptible2",
    "Exposed2",
    "Asymptomatic2",
    "PreSymptomatic2",
    "InfectedMild2",
    "InfectedSevere2",
    "Hospitalised2",
    "Recovered2",
    "Deceased2",
    "TotalInfected2",
    "TotalRemoved2",
  )


  override def getRows(): List[List[Any]] = {

    val graphProvider = context.graphProvider
    val label = "Person"
    val countMap  = mutable.HashMap.empty[String, Int]

    val nodes = graphProvider.fetchNodes(label)
    nodes.foreach(node => {
      val infectedState = node.getParams.apply("infectionState").toString
      val existingCount = countMap.getOrElse(infectedState, 0)
      countMap.put(infectedState, existingCount + 1)

      val infectedState2 = node.getParams.apply("infectionState2").toString
      val existingCount2 = countMap.getOrElse(infectedState2, 0)
      countMap.put(infectedState2, existingCount2 + 1)
    })

    val row = List(
      context.getCurrentStep*Disease.dt,
      countMap.getOrElse(Susceptible.toString, 0),
      countMap.getOrElse(Exposed.toString, 0),
      countMap.getOrElse(Asymptomatic.toString, 0),
      countMap.getOrElse(PreSymptomatic.toString, 0),
      countMap.getOrElse(InfectedMild.toString, 0),
      countMap.getOrElse(InfectedSevere.toString, 0),
      countMap.getOrElse(Hospitalised.toString, 0),
      countMap.getOrElse(Recovered.toString, 0),
      countMap.getOrElse(Dead.toString, 0),
      countMap.getOrElse(Asymptomatic.toString, 0)+countMap.getOrElse(PreSymptomatic.toString, 0) + countMap.getOrElse(InfectedMild.toString, 0) + countMap.getOrElse(InfectedSevere.toString, 0) +countMap.getOrElse(Hospitalised.toString, 0),
      countMap.getOrElse(Recovered.toString, 0) + countMap.getOrElse(Dead.toString, 0),
      "",
      "",
      countMap.getOrElse(Susceptible2.toString, 0),
      countMap.getOrElse(Exposed2.toString, 0),
      countMap.getOrElse(Asymptomatic2.toString, 0),
      countMap.getOrElse(PreSymptomatic2.toString, 0),
      countMap.getOrElse(InfectedMild2.toString, 0),
      countMap.getOrElse(InfectedSevere2.toString, 0),
      countMap.getOrElse(Hospitalised2.toString, 0),
      countMap.getOrElse(Recovered2.toString, 0),
      countMap.getOrElse(Dead2.toString, 0),
      countMap.getOrElse(Asymptomatic2.toString, 0)+countMap.getOrElse(PreSymptomatic2.toString, 0) + countMap.getOrElse(InfectedMild2.toString, 0) + countMap.getOrElse(InfectedSevere2.toString, 0) +countMap.getOrElse(Hospitalised2.toString, 0),
      countMap.getOrElse(Recovered2.toString, 0) + countMap.getOrElse(Dead2.toString, 0),
    )
    return List(row)

  }

}