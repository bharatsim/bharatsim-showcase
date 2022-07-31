package schools.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Network, StatefulAgent}
import schools.{Disease, Main}
import schools.InfectionStatus.{Asymptomatic, Hospitalised, InfectedMild, InfectedSevere, Presymptomatic}
import schools.models.Person

import scala.collection.mutable.ListBuffer

case class SusceptibleState() extends State {

  var agentGamma: Double = 0

  private def exitSusceptible(context: Context, agent: StatefulAgent): Boolean = {

    if (agent.asInstanceOf[Person].isSusceptible) {

      val agentBeta = rampedUpBeta(agent.asInstanceOf[Person].beta, t = context.getCurrentStep * Disease.dt, agent = agent)

      agentGamma = if (agent.asInstanceOf[Person].vaccineShots == 0) agent.asInstanceOf[Person].gamma
                   else vaccinatedParameter(context, agent, agent.asInstanceOf[Person].gamma, Disease.vaccinatedGammaFractionalIncrease_firstShot, Disease.vaccinatedGammaFractionalIncrease_secondShot)

      val schedule = context.fetchScheduleFor(agent.asInstanceOf[Person]).get

      val currentStep = context.getCurrentStep
      val placeType: String = schedule.getForStep(currentStep + 1)
      if (placeType == null) {
        println("No place")
      }
      val places = agent.getConnections(agent.getRelation(placeType).get).toList
      if (places.nonEmpty) {
        val place = places.head
        val decodedPlace = agent.asInstanceOf[Person].decodeNode(placeType, place)
        val infectedFraction = fetchInfectedFraction(decodedPlace, placeType, context)

        val r = Main.splittableRandom.nextDouble()
        val agentGetsInfected = r < (agentBeta * infectedFraction + Disease.backgroundFOI(context.getCurrentStep * Disease.dt)) * Disease.dt

        // Are they infected by the background FOI?
        val agentGetsInfectedByFOI = agentBeta * infectedFraction * Disease.dt < r && r < (agentBeta * infectedFraction + Disease.backgroundFOI(context.getCurrentStep * Disease.dt)) * Disease.dt

        if (infectedFraction > 0 && agentGetsInfected) {
          val peopleHere = decodedPlace.getConnections(decodedPlace.getRelation[Person]().get)
          val infectedHere = new ListBuffer[Person]()

          peopleHere.foreach(node => {
            val person = node.as[Person]
            if (person.isAsymptomatic || person.isPresymptomatic || person.isInfectedMild || person.isInfectedSevere || person.isHospitalised) {
              infectedHere += person
            }
          })

          val infectedHereList = infectedHere.toList
          val infectingAgent = infectedHereList(Main.splittableRandom.nextInt(infectedHereList.size))

          infectingAgent.updateParam("infectedPeople", infectingAgent.infectedPeople + 1)

          agent.updateParam("wasInfectedAt", placeType)
          agent.updateParam("wasInfectedBy", infectingAgent.infectionState.toString)

        }
        else if (agentGetsInfectedByFOI) {
          agent.updateParam("wasInfectedAt", placeType)
          agent.updateParam("wasInfectedBy", "FOI")
        }

        return agentGetsInfected
      }

    }
    false
  }

  def vaccinatedParameter(context: Context, agent: StatefulAgent, parameter: Double, fractionalIncrease_firstShot: Double, fractionalIncrease_secondShot: Double, rampUpTime: Double = 14.0): Double = {

    val t = context.getCurrentStep * Disease.dt

    if (agent.asInstanceOf[Person].vaccineShots == 1) {
      val vday = agent.asInstanceOf[Person].receivedFirstShotOn
      List(parameter + (1 - (fractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - parameter) - parameter) * (t - vday) / rampUpTime, parameter + (1 - (fractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - parameter) - parameter)).min
    }
    else if (agent.asInstanceOf[Person].vaccineShots == 2) {
      val vday = agent.asInstanceOf[Person].receivedSecondShotOn
      val parameter2 = parameter + (1 - (fractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - parameter) - parameter) //parameter + fractionalIncrease_firstShot*(1 - parameter)
      List(parameter2 + (1 - (fractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - parameter2) - parameter2) * (t - vday) / rampUpTime, parameter2 + (1 - (fractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - parameter2) - parameter2)).min
    }
    else {
      parameter
    }

  }

  private def rampedUpBeta(beta: Double, t: Double, agent: StatefulAgent, reduction: Double = 0.6, rampUpTime: Double = 90, vaccineRampUpTime: Double = 14): Double = {

    val shot = agent.asInstanceOf[Person].vaccineShots

    val agentBeta = if (shot == 1) {
      val vday = agent.asInstanceOf[Person].receivedFirstShotOn
      List(List(beta + (beta * Disease.vaccinatedBetaMultiplier_firstShot - beta) * (t - vday) / vaccineRampUpTime, beta * Disease.vaccinatedBetaMultiplier_firstShot).max, beta).min
    }
    else if (shot == 2) {
      val vday = agent.asInstanceOf[Person].receivedSecondShotOn
      val beta2 = beta * Disease.vaccinatedBetaMultiplier_firstShot
      List(List(beta + (beta2 * Disease.vaccinatedBetaMultiplier_secondShot - beta2) * (t - vday) / vaccineRampUpTime, beta2 * Disease.vaccinatedBetaMultiplier_secondShot).max, beta2).min
    }
    else beta

    if (Disease.rampUpBeta) {
      List((1 - reduction) * agentBeta + reduction * t / rampUpTime, agentBeta).min
    }
    else {
      agentBeta
    }

  }

  // Only looping over people associated with this location to find those currently here
  private def fetchInfectedFraction(decodedPlace: Network, placeType: String, context: Context): Double = {
    val cache = context.perTickCache

    val tuple = (placeType, decodedPlace.internalId)
    cache.getOrUpdate(tuple, () => fetchFromStore(decodedPlace, placeType)).asInstanceOf[Double]
  }

  private def fetchFromStore(decodedPlace: Network, placeType: String): Double = {
    val infectedPattern = ("infectionState" equ Asymptomatic) or ("infectionState" equ Presymptomatic) or ("infectionState" equ InfectedMild) or ("infectionState" equ InfectedSevere) or ("infectionState" equ Hospitalised)
    val total = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType))
    val infectedUnvaccinated = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("vaccinationStatus" equ false) and ("currentLocation" equ placeType) and infectedPattern)
    val infectedVaccinated = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("vaccinationStatus" equ true) and ("currentLocation" equ placeType) and infectedPattern)

    if (total == 0.0)
      return 0.0

    (infectedUnvaccinated.toDouble + (infectedVaccinated.toDouble * (1 - Disease.fractionalTransmissionReduction))) / total.toDouble
  }


  addTransition(
    when = exitSusceptible,
    to = context => ExposedState(context.getCurrentStep + Disease.inverse_dt * Disease.exposedDurationProbabilityDistribution.sample(), Main.splittableRandom.nextDouble() < agentGamma)
  )


}
