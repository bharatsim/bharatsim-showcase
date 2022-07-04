package multipleStrains.models

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Agent, Network}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import multipleStrains.InfectionStatus._
import multipleStrains.{Disease, Main}
import multipleStrains.Main.{firstShotsAvailableThisTick, prevaccinateFamilies, reinfectionRisk, secondShotsAvailableThisTick, vaccinatePeople}

case class Person(
    id: Long,
    age: Int,
    infectionState: InfectionStatus,
    infectionState2: InfectionStatus,
    isEssentialWorker: Boolean,
    violateLockdown: Boolean,
    village_town: String,
    lat: String,
    long: String,
    isEmployee: Boolean,
    isStudent: Boolean,
    isTeacher: Boolean,
    isHomebound: Boolean,
    gamma1: Double,
    gamma2: Double,
    betaMultiplier: Double,
    gammaMultiplier: Double,
    gammaMultiplier2: Double,
    exitTime: Double,
    exitTime2: Double,
    currentLocation: String = "",
    prevaccinate: Boolean = false,
    vaccinationStatus: Boolean = false,
    vaccineShots: Int = 0,
    shouldGetFirstShot: Boolean = false,
    shouldGetSecondShot: Boolean = false,
    receivedFirstShotOn: Double = 10000,
    receivedSecondShotOn: Double = 10000,
    gamma1MaxFirstShot: Double = -1.0,
    gamma1MaxSecondShot: Double = -1.0,
    gamma2MaxFirstShot: Double = -1.0,
    gamma2MaxSecondShot: Double = -1.0,
    infectedPeople: Int = 0,
    wasInfectedAt: String = "",
    wasInfectedBy: String = ""
) extends Agent {

  def decodeNode(classType: String, node: GraphNode): Network = {
    classType match {
      case "Home" => node.as[Home]
      case "Office" => node.as[Office]
      case "School" => node.as[School]
      case "ClassRoom" => node.as[ClassRoom]
      case "Hospital" => node.as[Hospital]
    }
  }

  private def fetchInfected(decodedPlace: Network, place: String, context: Context): (Float, Float) = {
    val cache = context.perTickCache
    val tuple = (place, decodedPlace.internalId)
    cache.getOrUpdate(tuple, () => fetchInfectedFromStore(decodedPlace)).asInstanceOf[(Float, Float)]
  }

  private def fetchInfectedFromStore(decodedPlace: Network): (Float, Float) = {
    val infectedPattern =
      ("infectionState" equ InfectedMild) or ("infectionState" equ InfectedSevere) or ("infectionState" equ Asymptomatic) or ("infectionState" equ PreSymptomatic)

    val infectedPattern2 =
      ("infectionState2" equ InfectedMild2) or ("infectionState2" equ InfectedSevere2) or ("infectionState2" equ Asymptomatic2) or ("infectionState2" equ PreSymptomatic2)

    val N = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get)

    val inf = decodedPlace.getConnectionCount(
      decodedPlace.getRelation[Person]().get,
      infectedPattern
    )

    val infectedFraction1 = inf.toFloat / N.toFloat

    val inf2 = decodedPlace.getConnectionCount(
      decodedPlace.getRelation[Person]().get,
      infectedPattern2
    )
    val infectedFraction2 = inf2.toFloat / N.toFloat

    (infectedFraction1, infectedFraction2)
  }

  private def exitSusceptible(context: Context): Unit = {
    if (this.isSusceptible || this.isSusceptible2) {

      val schedule = context.fetchScheduleFor(this).get

      val currentStep = context.getCurrentStep
      val placeType: String = schedule.getForStep(currentStep)

      val places = this.getConnections(this.getRelation(placeType).get).toList
      if (places.nonEmpty) {
        val place = places.head
        val decodedPlace = this.decodeNode(placeType, place)

        var multiplier  = 1.0
        var multiplier2 = 1.0

         if (!this.isSusceptible2) { // If they've been infected with strain 2
           multiplier = reinfectionRisk // they can't be infected by strain 1
         } else if (!this.isSusceptible) { // If they've been infected with strain 1
           multiplier2 = reinfectionRisk // they can't be infected by strain 2
         }

        val infectedFractionTuple = fetchInfected(decodedPlace, placeType, context)

        val infectedFraction = infectedFractionTuple._1
        val infectedFraction2 = infectedFractionTuple._2

        if (this.isSusceptible && multiplier > 0) {
          val agentBeta1: Double = multiplier * this.betaMultiplier * Disease.ageStratifiedBetaMultiplier.getOrElse(roundToAgeRange(age), Disease.ageStratifiedBetaMultiplier(99)) * Disease.beta

          val shouldGetInfected = biasedCoinToss(agentBeta1 * Disease.dt * infectedFraction)
          if (shouldGetInfected) {
            updateParam("infectionState", Exposed)
            updateParam(
              "exitTime",
              context.getCurrentStep + Disease.exposedDurationProbabilityDistribution.sample() * Disease.inverse_dt
            )
//            updateParam("gammaMultiplier2", Disease.vaccinatedGammaFractionalIncrease)
          }
        }

        if (this.isSusceptible2 && multiplier2 > 0) {
          val agentBeta2: Double = multiplier2 * this.betaMultiplier * Disease.ageStratifiedBetaMultiplier.getOrElse(roundToAgeRange(age), Disease.ageStratifiedBetaMultiplier(99)) * Disease.beta2

          val shouldGetInfected = biasedCoinToss(agentBeta2 * Disease.dt * infectedFraction2)
          if (shouldGetInfected) {
            updateParam("infectionState2", Exposed2)
            updateParam(
              "exitTime2",
              context.getCurrentStep + Disease.exposedDurationProbabilityDistribution.sample() * Disease.inverse_dt
            )
//            updateParam("gammaMultiplier", Disease.vaccinatedGammaFractionalIncrease)
          }
        }
      }
    }
  }

  private def exitExposed(context: Context): Unit = {
    if (this.isExposed && context.getCurrentStep >= this.exitTime) {
      val agentGamma1: Double = this.gamma1
      if (biasedCoinToss(agentGamma1 * (1 + this.gammaMultiplier))) {
        updateParam("infectionState", Asymptomatic)
        updateParam(
          "exitTime",
          context.getCurrentStep + Disease.asymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
      else {
        updateParam("infectionState", PreSymptomatic)
        updateParam(
          "exitTime",
          context.getCurrentStep + Disease.presymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
    }

    if (this.isExposed2 && context.getCurrentStep >= this.exitTime2) {
      val agentGamma2: Double = this.gamma2
      if (biasedCoinToss(agentGamma2 * (1 + this.gammaMultiplier2))) {
        updateParam("infectionState2", Asymptomatic2)
        updateParam(
          "exitTime2",
          context.getCurrentStep + Disease.asymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
      else {
        updateParam("infectionState2", PreSymptomatic2)
        updateParam(
          "exitTime2",
          context.getCurrentStep + Disease.presymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
    }
  }

  private def exitAsymptomatic(context: Context): Unit = {
    if (this.isAsymptomatic && context.getCurrentStep >= this.exitTime) {
      updateParam("infectionState", Recovered)
      updateParam("exitTime", 999999.0)
    }

    if (this.isAsymptomatic2 && context.getCurrentStep >= this.exitTime2) {
      updateParam("infectionState2", Recovered2)
      updateParam("exitTime2", 999999.0)
    }
  }

  private def exitPresymptomatic(context: Context): Unit = {
    if (this.isPresymptomatic && context.getCurrentStep >= this.exitTime) {
      val agentOneMinusDelta1: Double = Disease.ageStratifiedOneMinusDelta.getOrElse(roundToAgeRange(age), Disease.ageStratifiedOneMinusDelta(99))
      if (biasedCoinToss(agentOneMinusDelta1)) {
        updateParam("infectionState", InfectedSevere)
        updateParam(
          "exitTime",
          context.getCurrentStep + Disease.severeSymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
      else {
        updateParam("infectionState", InfectedMild)
        updateParam(
          "exitTime",
          context.getCurrentStep + Disease.mildSymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
    }

    if (this.isPresymptomatic2 && context.getCurrentStep >= this.exitTime2) {
      val agentOneMinusDelta2: Double = Disease.ageStratifiedOneMinusDelta.getOrElse(roundToAgeRange(age), Disease.ageStratifiedOneMinusDelta(99))
      if (biasedCoinToss(agentOneMinusDelta2)) {
        updateParam("infectionState2", InfectedSevere2)
        updateParam(
          "exitTime2",
          context.getCurrentStep + Disease.severeSymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
      else {
        updateParam("infectionState2", InfectedMild2)
        updateParam(
          "exitTime2",
          context.getCurrentStep + Disease.mildSymptomaticDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
      }
    }
  }

  private def exitInfectedMild(context: Context): Unit = {
    if (this.isInfectedMild && context.getCurrentStep >= this.exitTime) {
      updateParam("infectionState", Recovered)
      updateParam("exitTime", 999999.0)
    }

    if (this.isInfectedMild2 && context.getCurrentStep >= this.exitTime2) {
      updateParam("infectionState2", Recovered2)
      updateParam("exitTime2", 999999.0)
    }
  }


  private def exitInfectedSevere(context: Context): Unit = {
    if (this.isInfectedSevere && context.getCurrentStep >= this.exitTime) {
      updateParam("infectionState", Hospitalised)
      updateParam(
          "exitTime",
          context.getCurrentStep + Disease.hospitalisedDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
    }

    if (this.isInfectedSevere2 && context.getCurrentStep >= this.exitTime2) {
      updateParam("infectionState2", Hospitalised2)
      updateParam(
          "exitTime2",
          context.getCurrentStep + Disease.hospitalisedDurationProbabilityDistribution.sample() * Disease.inverse_dt
        )
    }
  }

  private def exitHospitalised(context: Context): Unit = {
    if (this.isHospitalised && context.getCurrentStep >= this.exitTime) {
      val agentSigma1: Double = Disease.ageStratifiedSigma.getOrElse(roundToAgeRange(age), Disease.ageStratifiedSigma(99))
      if (!biasedCoinToss(agentSigma1)) {
        updateParam("infectionState", Recovered)
        updateParam("exitTime", 999999.0)
        return
      }
      else {
        updateParam("infectionState", Dead)
        updateParam("exitTime", 999999.0)
      }
    }

    if (this.isHospitalised2 && context.getCurrentStep >= this.exitTime2) {
      val agentSigma2: Double = Disease.ageStratifiedSigma.getOrElse(roundToAgeRange(age), Disease.ageStratifiedSigma(99))
      if (!biasedCoinToss(agentSigma2)) {
        updateParam("infectionState2", Recovered2)
        updateParam("exitTime2", 999999.0)
      }
      else {
        updateParam("infectionState2", Dead2)
        updateParam("exitTime2", 999999.0)
      }
    }
  }

  def isSusceptible: Boolean = infectionState == Susceptible

  def isExposed: Boolean = infectionState == Exposed

  def isAsymptomatic: Boolean = infectionState == Asymptomatic

  def isPresymptomatic: Boolean = infectionState == PreSymptomatic

  def isInfectedMild: Boolean = infectionState == InfectedMild

  def isInfectedSevere: Boolean = infectionState == InfectedSevere

  def isHospitalised: Boolean = infectionState == Hospitalised

  def isRecovered: Boolean = infectionState == Recovered

  def isSusceptible2: Boolean = infectionState2 == Susceptible2

  def isExposed2: Boolean = infectionState2 == Exposed2

  def isAsymptomatic2: Boolean = infectionState2 == Asymptomatic2

  def isPresymptomatic2: Boolean = infectionState2 == PreSymptomatic2

  def isInfectedMild2: Boolean = infectionState2 == InfectedMild2

  def isInfectedSevere2: Boolean = infectionState2 == InfectedSevere2
  
  def isHospitalised2: Boolean = infectionState2 == Hospitalised2

  def isRecovered2: Boolean = infectionState2 == Recovered2

  def hasNoSymptoms: Boolean = !(isInfectedMild || isInfectedSevere || isHospitalised || isInfectedMild2 || isInfectedSevere2 || isHospitalised2)


  addBehaviour(exitSusceptible)
  addBehaviour(exitExposed)
  addBehaviour(exitAsymptomatic)
  addBehaviour(exitPresymptomatic)
  addBehaviour(exitInfectedMild)
  addBehaviour(exitInfectedSevere)
  addBehaviour(exitHospitalised)

  def vaccinatePerson(context: Context): Unit = {

    val t: Double = context.getCurrentStep * Main.dt
    if (shouldGetVaccine(t, roundToAgeRange(age))) {

      // Potential first shots
      if (!this.shouldGetFirstShot && this.vaccineShots == 0 && firstShotsAvailableThisTick > 0) {
        firstShotsAvailableThisTick -= 1
        updateParam("shouldGetFirstShot", true)
      }

      // Potential second shots
      if (!this.shouldGetSecondShot && this.vaccineShots == 1 && t >= receivedFirstShotOn + Disease.Delta && secondShotsAvailableThisTick > 0) {
        secondShotsAvailableThisTick -= 1
        updateParam("shouldGetSecondShot", true)
      }
    }

  }

  private def roundToAgeRange(age: Int): Int = { //Copied from BharatSim master (21.06.21)
    (age / 10) * 10 + 9
  }

  def shouldGetVaccine(t: Double, age: Int): Boolean = {

    if (prevaccinateFamilies && this.prevaccinate && this.vaccineShots == 0) {
      // That means this person should've been prevaccinated but wasn't, so vaccinate them now!
      return true
    }

    if (t < Disease.phase1_endDate) {
      val target_ages = Disease.phase1

      if (target_ages.contains(age)) {
        return hasNoSymptoms
      }
      return false
    }
    else if (t >= Disease.phase1_endDate && t < Disease.phase2_endDate) {
      val target_ages = Disease.phase2
      if (target_ages.contains(age)) {
        return hasNoSymptoms
      }
      return false
    }
    else if (t >= Disease.phase2_endDate) {
      val target_ages = Disease.phase3
      if (target_ages.contains(age)) {
        return hasNoSymptoms
      }
      return false
    }
    false
  }

  private def updateCurrentLocation(context: Context): Unit = {
    val currentPlaceOption = getCurrentPlace(context)
    currentPlaceOption match {
      case Some(x) => {
        if (this.currentLocation != x) {
          updateParam("currentLocation", x)
        }
      }
      case _ =>
    }
  }

  private def getCurrentPlace(context: Context): Option[String] = {
    val schedule = context.fetchScheduleFor(this).get
    val currentStep = context.getCurrentStep
    val placeType: String = schedule.getForStep(currentStep)

    Some(placeType)
  }


  if (vaccinatePeople) {
    addBehaviour(vaccinatePerson)
  }

  addBehaviour(updateCurrentLocation)


  addRelation[Home]("STAYS_AT")
  addRelation[Office]("EMPLOYED_BY")
  addRelation[School]("GOES_TO")
  addRelation[ClassRoom]("LEARNS_IN")
  addRelation[Hospital]("TREATED_AT")
}
