package schools.models

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.models.{Network, StatefulAgent}
import schools.{Disease, Main}
import schools.InfectionStatus._
import schools.Main.{firstShotsAvailableThisTick, prevaccinateFamilies, secondShotsAvailableThisTick, vaccinatePeople}
import schools.diseaseStates._

case class Person(agentId: Long,
                  age: Int,
                  infectionDay: Int,
                  infectedAt: Int,
                  enteredCurrentState: Int,
                  infectionState: InfectionStatus,
                  beta: Double,
                  gamma: Double,
                  delta: Double,
                  sigma: Double,
                  isEssentialWorker: Boolean,
                  violateLockdown: Boolean,
                  villageTown: String,
                  lat: String,
                  long: String,
                  isEmployee: Boolean,
                  isStudent: Boolean,
                  isTeacher: Boolean,
                  isHomebound: Boolean,
                  currentLocation: String = "",
                  prevaccinate: Boolean = false,
                  vaccinationStatus: Boolean = false,
                  vaccineShots: Int = 0,
                  shouldGetFirstShot: Boolean = false,
                  shouldGetSecondShot: Boolean = false,
                  receivedFirstShotOn: Double = 10000,
                  receivedSecondShotOn: Double = 10000,
                  gammaMaxFirstShot: Double = -1.0,
                  gammaMaxSecondShot: Double = -1.0,
                  infectedPeople: Int = 0,
                  wasInfectedAt: String = "",
                  wasInfectedBy: String = ""
                 ) extends StatefulAgent {

  def isExposed: Boolean = activeState.isInstanceOf[ExposedState]

  def isAsymptomatic: Boolean = activeState.isInstanceOf[AsymptomaticState]

  def isPresymptomatic: Boolean = activeState.isInstanceOf[PresymptomaticState]

  def isInfected: Boolean = activeState.isInstanceOf[AsymptomaticState] || activeState.isInstanceOf[PresymptomaticState] ||
    activeState.isInstanceOf[InfectedMildState] || activeState.isInstanceOf[InfectedSevereState] ||
    activeState.isInstanceOf[HospitalisedState]

  def isVaccinated: Boolean = this.vaccinationStatus

  def isSusceptible: Boolean = activeState.isInstanceOf[SusceptibleState]

  def isRecovered: Boolean = activeState.isInstanceOf[RecoveredState]

  def decodeNode(classType: String, node: GraphNode): Network = {
    classType match {
      case "Home" => node.as[Home]
      case "Office" => node.as[Office]
      case "School" => node.as[School]
      case "ClassRoom" => node.as[ClassRoom]
      case "Hospital" => node.as[Hospital]
    }
  }

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
        return !(isInfectedMild || isInfectedSevere || isHospitalised)
      }
      return false
    }
    else if (t >= Disease.phase1_endDate && t < Disease.phase2_endDate) {
      val target_ages = Disease.phase2
      if (target_ages.contains(age)) {
        return !(isInfectedMild || isInfectedSevere || isHospitalised)
      }
      false
    }
    else if (t >= Disease.phase2_endDate) {
      val target_ages = Disease.phase3
      if (target_ages.contains(age)) {
        return !(isInfectedMild || isInfectedSevere || isHospitalised)
      }
      return false
    }
    false
  }

  def isHospitalised: Boolean = activeState.isInstanceOf[HospitalisedState]

  def isInfectedMild: Boolean = activeState.isInstanceOf[InfectedMildState]

  def isInfectedSevere: Boolean = activeState.isInstanceOf[InfectedSevereState]

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
