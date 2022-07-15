package schools

import com.bharatsim.engine.ContextBuilder._
import com.bharatsim.engine._
import com.bharatsim.engine.actions.StopSimulation
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.dsl.SyntaxHelpers.ScheduleMaker
import com.bharatsim.engine.execution.Simulation
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.ingestion.{GraphData, Relation}
import com.bharatsim.engine.graph.patternMatcher.EmptyPattern
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.intervention.{Intervention, SingleInvocationIntervention}
import com.bharatsim.engine.listeners.{CsvOutputGenerator, SimulationListenerRegistry}
import com.bharatsim.engine.models.Agent
import com.typesafe.scalalogging.LazyLogging
import schools.InfectionStatus._
import schools.diseaseStates._
import schools.models._


object Main extends LazyLogging {

  var initialExposedNumber = 3000 // toDo: Don't hardcode it!

  final val splittableRandom: RandomNumberGenerator = RandomNumberGenerator()

  var vaccinesAdministered: Int = 0
  var vaccinesAdministeredThisTick: Int = 0
  var vaccinesAvailableToday: Int = 0
  var firstShotsAvailableThisTick: Int = 0
  var secondShotsAvailableThisTick: Int = 0

  var ageWiseVaccinesAdministered: Array[Int] = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  private var vaccinationStarted: Double = 0
  private var ingestedPopulation = 0
  private var schoolsClosedOn: Double = 0
  private var schoolsOpenedOn: Double = 0
  private var lockdownStartedOn: Double = 0

  def parseArgs(args: Array[String]): Unit = {

    for (arg <- args) {
      val a = arg.split("=")

      if(a.length!=2){
        throw new Exception(s"Unsupported syntax for argument: \""+arg+"\". Flag syntax is `name=value`, without spaces.")
      }
      else{
        val key = a(0).toUpperCase
        val value = a(1)

        key match{
          case "INPUT"  => { Disease.inputPath = value; logger.info("Set input path to "+Disease.inputPath) }
          case "OUTPUT" => { Disease.outputPath = value; logger.info("Set output path to "+Disease.outputPath)  }
          case "IR"     => { Disease.initialRecoveredFraction = value.toFloat / 100f; logger.info("Set initial recovered fraction to "+Disease.initialRecoveredFraction) }
          case "IV1"    => { Disease.prevaccinatedOneShotFraction = value.toFloat / 100; logger.info("Set initial one-shot vaccination fraction to "+Disease.prevaccinatedOneShotFraction) }
          case "IV2"    => { Disease.prevaccinatedTwoShotFraction = value.toFloat / 100; logger.info("Set initial two-shot vaccination fraction to "+Disease.prevaccinatedTwoShotFraction) }
          case "DVR"    => { Disease.vaccinationRate = value.toDouble / 100; logger.info("Set daily vaccination rate to "+Disease.vaccinationRate) }
          case "VT"     => { Disease.vaccinationTriggerFraction = value.toFloat / 100; logger.info("Set vaccination trigger fraction to "+Disease.vaccinationTriggerFraction) }
          case "USA"    => { Disease.unlockSchoolsAt = value.toInt; logger.info("Set day to unlock schools at to "+Disease.unlockSchoolsAt) }
          case "LT"     => { Disease.lockdownTriggerFraction = value.toFloat / 100; logger.info("Set lockdown trigger fraction to "+Disease.lockdownTriggerFraction) }
          case "LAT"    => { Disease.lockdownAdherenceThreshold = value.toFloat / 100; logger.info("Set lockdown adherence threshold to "+Disease.lockdownAdherenceThreshold) }
          case "SEED"   => { if(value.toUpperCase=="WARD") { Disease.localizedWardInfections = true; logger.info("Set initial infections to single ward "+Disease.initialInfectedWard) }
                             else if(value.toUpperCase=="HOUSEHOLDS") { Disease.localizedHouseListInfections = true; logger.info("Set initial infections to household list: "+Disease.initialInfectedHouseholds) }}
          case _        => { throw new Exception(s"Unsupported flag: \""+key+"\". Available flags are \"INPUT\", \"OUTPUT\", \"IR\", \"IV1\", \"IV2\", \"DVR\", \"VT\", \"USA\", \"LT\", \"LAT\", \"SEED\".") }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var beforeCount = 0
    val simulation = Simulation()

    logger.info("Bare Beta = " + Disease.lambda_S)

    parseArgs(args)

    logger.info("Phase1 " + Disease.phase1 + " ends on day " + Disease.phase1_endDate)
    logger.info("Phase2 " + Disease.phase2 + " ends on day " + Disease.phase2_endDate)

    simulation.ingestData { implicit context =>
      ingestCSVData(Disease.inputPath, mapper)
      logger.debug("Ingestion done")
    }

    simulation.defineSimulation { implicit context =>
      ingestedPopulation = context.graphProvider.fetchCount("Person", EmptyPattern())

      if (Disease.prevaccinatedOneShotFraction != 0) {
        prevaccination(shot = 1)
      }

      if (Disease.prevaccinatedTwoShotFraction != 0) {
        prevaccination(shot = 2)
      }

      if (Disease.vaccinationRate != 0 && Disease.vaccinationTriggerFraction <= 1) {
        vaccination
      }

      if(Disease.unlockSchoolsAt > 0) {
        closeSchoolsUntil
      }

      if (Disease.lockdownTriggerFraction <= 1) {
        lockdown
      }

      create12HourSchedules()

      registerAction(
        StopSimulation,
        (c: Context) => {
          c.getCurrentStep >= (200 * Disease.inverse_dt)
        }
      )

      beforeCount = getInfectedCount(context)

      registerAgent[Person]

      registerState[SusceptibleState]
      registerState[ExposedState]
      registerState[AsymptomaticState]
      registerState[PresymptomaticState]
      registerState[InfectedMildState]
      registerState[InfectedSevereState]
      registerState[RecoveredState]
      registerState[HospitalisedState]
      registerState[DeadState]

      val rn = splittableRandom.nextDouble()
      val closeschoolslabel = if (Disease.unlockSchoolsAt > 0) 1 else 0
      val lockdownlabel = if (Disease.lockdownTriggerFraction <= 1) 1 else 0

      var label = "_IR_" + (Disease.initialRecoveredFraction * 100).toInt + "_IV_" + (Disease.prevaccinatedOneShotFraction * 100).toInt + "_" + (Disease.prevaccinatedTwoShotFraction * 100).toInt

      label += "_VR_" + (Disease.vaccinationRate * 100)
      label += "_closeSchools_" + closeschoolslabel + "_unlockSchoolsAt_" + Disease.unlockSchoolsAt
      label += "_lockdown_" + lockdownlabel
      // todo add lockdown label on when lockdown started (if possible)

      SimulationListenerRegistry.register(
        new CsvOutputGenerator(Disease.outputPath + "total_output" + label + "_" + rn + ".csv", new SIROutput(context))
      )
//      SimulationListenerRegistry.register(
//        new CsvOutputGenerator(Disease.outputPath + "agewise_output" + label + "_" + rn + ".csv", new SIRAgewiseOutput(context))
//      )
//      SimulationListenerRegistry.register(
//        new CsvOutputGenerator(Disease.outputPath + "infectioninfo_output" + label + "_" + rn + ".csv", new InfectionInfoOutput(context))
//      )
      SimulationListenerRegistry.register(
        new CsvOutputGenerator(Disease.outputPath + "GIS_output" + label + "_" + rn + ".csv", new GISOutputSpec(context))
      )
      SimulationListenerRegistry.register(
        new CsvOutputGenerator(Disease.outputPath + "Wardwise_output" + label + "_" + rn + ".csv", new WardwiseOutputSpec(context))
      )
    }

    simulation.onCompleteSimulation { implicit context =>
      printStats(beforeCount)
      teardown()
    }

    val startTime = System.currentTimeMillis()
    simulation.run()
    val endTime = System.currentTimeMillis()
    logger.info("Total time: {} s", (endTime - startTime) / 1000)

  }

  def mapper(row: Map[String, String]): GraphData = {
    val agentID = row("Agent_ID").toLong
    val age = row("Age").toInt
    val isEssentialWorker = row("essential_worker").toInt == 1
    val violateLockdown = row("Adherence_to_Intervention").toFloat > Disease.lockdownAdherenceThreshold

    val villageTown = row("AdminUnitName")
    val lat = row("H_Lat")
    val long = row("H_Lon")
    val homeId = row("HHID").toLong
    val home = Home(homeId)
    val data = GraphData()
    val staysAt = Relation[Person, Home](agentID, "STAYS_AT", homeId)
    val houses = Relation[Home, Person](homeId, "HOUSES", agentID)

    val infectionState = if (Disease.localizedWardInfections) setInitialPopulation(villageTown) else if (Disease.localizedHouseListInfections) setInitialPopulation(homeId) else setInitialPopulation()

    val schoolID = row("school_id").toLong
    val officeID = row("WorkPlaceID").toLong // /100 // ToDo: Fix the synthetic population so that we don't need to do this
    // ToDo: Perhaps add an FOI?

    val isEmployee: Boolean = officeID > 0
    val isTeacher: Boolean = officeID > 0 && schoolID > 0 && officeID == schoolID
    val isStudent: Boolean = schoolID > 0 && !isTeacher
    val isHomebound: Boolean = officeID == 0 && schoolID == 0

    if (!isEmployee && !isTeacher && !isStudent && !isHomebound) {
      print("Agent " + agentID)
    }

    val agentBeta: Double = Disease.ageStratifiedBetaMultiplier.getOrElse(roundToAgeRange(age), Disease.ageStratifiedBetaMultiplier(99)) * Disease.lambda_S
    val agentGamma: Double = 1.0 - Disease.ageStratifiedOneMinusGamma.getOrElse(roundToAgeRange(age), Disease.ageStratifiedOneMinusGamma(99))
    val agentDelta: Double = 1.0 - Disease.ageStratifiedOneMinusDelta.getOrElse(roundToAgeRange(age), Disease.ageStratifiedOneMinusDelta(99))
    val agentSigma: Double = Disease.ageStratifiedSigma.getOrElse(roundToAgeRange(age), Disease.ageStratifiedSigma(99))

    // Prevaccinate 80% of the 60+, and use the remaining to prevaccinate the population randomly
    val prevaccinate = (age > 60 && splittableRandom.nextDouble() < 0.8)

    // Alternatively, one could prevaccinate family members and teachers
    //    val prevaccinate = (row("fam_mem").toInt == 1 || isTeacher) && prevaccinateFamilies


    val agent: Person = Person(
      agentID,
      age,
      infectionDay = 0,
      infectedAt = 0,
      enteredCurrentState = 0,
      InfectionStatus.withName(infectionState),
      beta = agentBeta,
      gamma = agentGamma,
      delta = agentDelta,
      sigma = agentSigma,
      isEssentialWorker,
      violateLockdown,
      villageTown,
      lat,
      long,
      isEmployee,
      isStudent,
      isTeacher,
      isHomebound,
      prevaccinate = prevaccinate
    )

    setCitizenInitialState(agent, infectionState)

    data.addNode(agentID, agent)
    data.addNode(homeId, home)
    data.addRelations(staysAt, houses)

    if (isEmployee && !isTeacher) {
      val office = Office(officeID)
      val employedBy = Relation[Person, Office](agentID, "EMPLOYED_BY", officeID)
      val employerOf = Relation[Office, Person](officeID, "EMPLOYER_OF", agentID)

      data.addNode(officeID, office)
      data.addRelations(employedBy, employerOf)
    }
    else if (isStudent || isTeacher) {
      val school = School(schoolID)
      val goesTo = Relation[Person, School](agentID, "GOES_TO", schoolID)
      val teaches = Relation[School, Person](schoolID, "TEACHES", agentID)

      data.addNode(schoolID, school)
      data.addRelations(teaches, goesTo)

      val schoolGrade = setSchoolGradeRandomly(age)
      val classRoomId = (schoolID.toString + "0" + schoolGrade).toLong
      val classroom = ClassRoom(classRoomId)
      val learnsIn = Relation[Person, ClassRoom](agentID, "LEARNS_IN", classRoomId)
      val classTeaches = Relation[ClassRoom, Person](classRoomId, "CLASS_TEACHES", agentID)

      data.addNode(classRoomId, classroom)
      data.addRelations(learnsIn, classTeaches)

    }
    else if (!isHomebound) {
      print("Error. Agent " + agentID + " not assigned a schedule.")
    }


    if (infectionState == Hospitalised.toString) {
      val hospitalId = 1
      val hospital = Hospital(hospitalId)

      val visitsHospital = Relation[Person, Hospital](agentID, "TREATED_AT", hospitalId)
      val hospitalVisited = Relation[Hospital, Person](hospitalId, "TREATS", agentID)

      data.addNode(hospitalId, hospital)
      data.addRelations(visitsHospital, hospitalVisited)
    }

    ingestedPopulation = ingestedPopulation + 1
    data
  }

  private def setInitialPopulation(houseID: Long): String = {
    if (Disease.initialInfectedHouseholds.contains(houseID)) {
      "Exposed"
    }
    else {
      val x = splittableRandom.nextDouble()
      if (x <= Disease.initialRecoveredFraction) {
        "Recovered"
      }
      else {
        "Susceptible"
      }
    }
  }

  private def setInitialPopulation(adminUnit: String): String = {
    if ((adminUnit == Disease.initialInfectedWard) && (initialExposedNumber > 0)) {
      initialExposedNumber -= 1
      "Exposed"
    }
    else {
      val x = splittableRandom.nextDouble()
      if (x <= Disease.initialRecoveredFraction) {
        "Recovered"
      }
      else {
        "Susceptible"
      }
    }
  }

  private def setInitialPopulation(): String = {
    val x = splittableRandom.nextDouble()
    if (x <= Disease.initialExposedFraction) {
      "Exposed"
    }
    else if (Disease.initialExposedFraction < x && x <= (Disease.initialRecoveredFraction + Disease.initialExposedFraction)) {
      "Recovered"
    }
    else {
      "Susceptible"
    }
  }

  private def setCitizenInitialState(citizen: Person, initialStatus: String): Unit = {
    initialStatus match {
      case "Susceptible" => citizen.setInitialState(SusceptibleState())
      case "Exposed" => citizen.setInitialState(ExposedState(Disease.inverse_dt * Disease.exposedDurationProbabilityDistribution.sample(), splittableRandom.nextDouble() < citizen.gamma))
      case "Asymptomatic" => citizen.setInitialState(AsymptomaticState(Disease.inverse_dt * Disease.asymptomaticDurationProbabilityDistribution.sample()))
      case "Presymptomatic" => citizen.setInitialState(PresymptomaticState(Disease.inverse_dt * Disease.presymptomaticDurationProbabilityDistribution.sample(), splittableRandom.nextDouble() < citizen.delta))
      case "InfectedMild" => citizen.setInitialState(InfectedMildState(Disease.inverse_dt * Disease.mildSymptomaticDurationProbabilityDistribution.sample()))
      case "InfectedSevere" => citizen.setInitialState(InfectedSevereState(Disease.inverse_dt * (Disease.severeSymptomaticDurationProbabilityDistribution.sample() + Disease.mildToSevereSymptomaticDurationProbabilityDistribution.sample()), toBeH = true)) // All SI go to H in IndSciSim
      case "Recovered" => citizen.setInitialState(RecoveredState())
      case "Hospitalised" => citizen.setInitialState(HospitalisedState(Disease.inverse_dt * Disease.hospitalisedDurationProbabilityDistribution.sample(), splittableRandom.nextDouble() < citizen.sigma))
      case "Dead" => citizen.setInitialState(DeadState())
      case _ => throw new Exception(s"Unsupported infection status: $initialStatus")
    }
  }

  private def roundToAgeRange(age: Int): Int = { //Copied from BharatSim master (21.06.21)
    (age / 10) * 10 + 9
  }

  private def setSchoolGradeRandomly(age: Int): String = {
    (splittableRandom.nextDouble() * Disease.nClassrooms).toInt.toString
  }

  private def create12HourSchedules()(implicit context: Context): Unit = {

    val EmployeeSchedule = (Disease.myDay, Disease.myTick)
      .add[Home](0, 0)
      .add[Office](1, 1)

    val StudentSchedule = (Disease.myDay, Disease.myTick)
      .add[Home](0, 0)
      .add[ClassRoom](1, 1)

    val TeacherSchedule = (Disease.myDay, Disease.myTick)
      .add[Home](0, 0)
      .add[ClassRoom](1, 1)

    val HospitalisedSchedule = (Disease.myDay, Disease.myTick)
      .add[Hospital](0, 1)

    val HomeboundSchedule = (Disease.myDay, Disease.myTick)
      .add[Home](0, 1)


    registerSchedules(
      (HospitalisedSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isHospitalised, 1),
      (TeacherSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isTeacher, 4),
      (EmployeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isEmployee && !agent.asInstanceOf[Person].isTeacher, 5),
      (StudentSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isStudent, 6),
      (HomeboundSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isHomebound, 7)
    )

  }

  private def closeSchoolsUntil(implicit context: Context): Unit = {

    var ActivatedAt = 0
    val interventionName = "lockSchools"
    val activationCondition = (context: Context) => {
      val result = context.getCurrentStep >= 0
      if (result) {
        schoolsClosedOn = context.getCurrentStep * Disease.dt
        schoolsOpenedOn = schoolsClosedOn
        logger.info("Schools closed on day " + schoolsClosedOn)
      }
      result
    }
    val deactivationCondition = (context: Context) => {
      val result = context.getCurrentStep >= Disease.unlockSchoolsAt * Disease.inverse_dt
      if (result) {
        val currentlyUnvaccinated = context.graphProvider.fetchCount("Person", ("vaccinationStatus" equ false)).toDouble
        logger.info("Schools opened on day " + schoolsOpenedOn + ", unvaccinated fraction " + currentlyUnvaccinated / ingestedPopulation)
      }
      result
    }
    val firstTimeExecution = (context: Context) => ActivatedAt = context.getCurrentStep

    val perTickAction = (context: Context) => {
      schoolsOpenedOn += Disease.dt
    }

    val intervention = SingleInvocationIntervention(interventionName, activationCondition, deactivationCondition, firstTimeExecution, whenActiveActionFunc = perTickAction) // Lock schools for 30 days

    registerIntervention(intervention)

    val schoolLockdownSchedule = (Disease.myDay, Disease.myTick)
      .add[Home](0, 1)

    registerSchedules(
      (schoolLockdownSchedule,
        (agent: Agent, context: Context) => {
          val isStudent = agent.asInstanceOf[Person].isStudent
          val isTeacher = agent.asInstanceOf[Person].isTeacher
          val schoolsLocked = context.activeInterventionNames.contains(interventionName)
          schoolsLocked && (isStudent || isTeacher)
        },
        3
      )
    )

  }

  private def prevaccination(shot: Int)(implicit context: Context): Unit = {

    val preVaccinesAvailable = if (shot == 1) (Disease.prevaccinatedOneShotFraction * ingestedPopulation).toInt else if (shot == 2) (Disease.prevaccinatedTwoShotFraction * ingestedPopulation).toInt else 0

    val preVaccinationFamilyIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", ("prevaccinate" equ true))

    val preVaccinationFamilyList = preVaccinationFamilyIterator.toList.take(preVaccinesAvailable)

    val family_prevaccinations = preVaccinationFamilyList.length
    var random_prevaccinations = 0

    val remaining_vaccines = preVaccinesAvailable - family_prevaccinations


    preVaccinationFamilyList.foreach(node => {

      val person = node.as[Person]

      val agentGamma = person.gamma
      val gammaMaxFirstShot = agentGamma + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - agentGamma) - agentGamma)
      val gammaMaxSecondShot = gammaMaxFirstShot + (1 - (Disease.vaccinatedGammaFractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - gammaMaxFirstShot) - gammaMaxFirstShot)

      person.updateParam("vaccinationStatus", true)
      if (shot == 1) {
        person.updateParam("vaccineShots", 1)
        person.updateParam("receivedFirstShotOn", -10000.0)

        person.updateParam("gammaMaxFirstShot", gammaMaxFirstShot)

        ageWiseVaccinesAdministered(floorAge(person.age)) += 1
        vaccinesAdministered = vaccinesAdministered + 1
      }
      else if (shot == 2) {
        person.updateParam("vaccineShots", 2)
        person.updateParam("receivedFirstShotOn", -10000.0)
        person.updateParam("receivedSecondShotOn", -10000.0)

        person.updateParam("gammaMaxFirstShot", gammaMaxFirstShot)
        person.updateParam("gammaMaxSecondShot", gammaMaxSecondShot)

        ageWiseVaccinesAdministered(floorAge(person.age)) += 1
        vaccinesAdministered = vaccinesAdministered + 1
      }

    })

    if (remaining_vaccines > 0) {

      val preVaccinationRandomIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", ("prevaccinate" equ false) and ("age" gte 18))

      val preVaccinationRandomList = preVaccinationRandomIterator.toList.take(remaining_vaccines)

      random_prevaccinations = remaining_vaccines

      preVaccinationRandomList.foreach(node => {

        val person = node.as[Person]

        val agentGamma = person.gamma
        val gammaMaxFirstShot = agentGamma + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - agentGamma) - agentGamma)
        val gammaMaxSecondShot = gammaMaxFirstShot + (1 - (Disease.vaccinatedGammaFractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - gammaMaxFirstShot) - gammaMaxFirstShot)

        person.updateParam("vaccinationStatus", true)
        if (shot == 1) {
          person.updateParam("vaccineShots", 1)
          person.updateParam("receivedFirstShotOn", -10000.0)

          person.updateParam("gammaMaxFirstShot", gammaMaxFirstShot)

          ageWiseVaccinesAdministered(floorAge(person.age)) += 1
          vaccinesAdministered = vaccinesAdministered + 1

        }
        else if (shot == 2) {
          person.updateParam("vaccineShots", 2)
          person.updateParam("receivedFirstShotOn", -10000.0)
          person.updateParam("receivedSecondShotOn", -10000.0)

          person.updateParam("gammaMaxFirstShot", gammaMaxFirstShot)
          person.updateParam("gammaMaxSecondShot", gammaMaxSecondShot)

          ageWiseVaccinesAdministered(floorAge(person.age)) += 1
          vaccinesAdministered = vaccinesAdministered + 1
        }
      })
    }

    logger.info("Prevaccinations done for shot " + shot + ": " + preVaccinesAvailable + " Prioritised: " + family_prevaccinations + " Random: " + random_prevaccinations + " Total: " + (family_prevaccinations + random_prevaccinations))


  }

  private def vaccination(implicit context: Context): Unit = {

    var ActivatedAt = 0
    val interventionName = "vaccination"
    val activationCondition = (context: Context) => {
//      val conditionMet = context.getCurrentStep >= 0
    val conditionMet = getInfectedCount(context) >= Disease.vaccinationTriggerFraction*ingestedPopulation
      if (conditionMet) {
        vaccinationStarted = context.getCurrentStep * Disease.dt
        logger.info("Vaccination started on day "+vaccinationStarted)
      }
      conditionMet
    }

    val firstTimeExecution = (context: Context) => ActivatedAt = context.getCurrentStep
    val deActivationCondition = (context: Context) => {
      vaccinesAdministered >= 2 * ingestedPopulation
    }

    val perTickAction = (context: Context) => {

      firstShotsAvailableThisTick = 2 * (Disease.vaccinationRate * ingestedPopulation * Disease.dt).toInt
      secondShotsAvailableThisTick = 2 * (Disease.vaccinationRate * ingestedPopulation * Disease.dt).toInt

      if (context.getCurrentStep % Disease.inverse_dt == 0) {

        var vaccinesAdministeredToday = 0
        vaccinesAvailableToday = (Disease.vaccinationRate * ingestedPopulation).toInt

        // VACCINE ADMINISTRATION: uses the following algorithm
        // divide the total vaccines such that firstShots:SecondShots :: (tau):(1-tau)
        // administer the vaccines to the people who are available to receive them
        // if there are leftover vaccines (eg: more 2nd doses available)
        // swap the remainder and re-administer (eg: give the excess 2nd doses to people who needed their 1st dose)
        // though the ratio will not necessarily be tau, this ensures it is close to it

        var nFirstShots: Int = (Disease.tau * Disease.vaccinationRate * ingestedPopulation).toInt
        var nSecondShots: Int = vaccinesAvailableToday - nFirstShots

        var wastedFirstShots: Int = 0
        var wastedSecondShots: Int = 0

        val potentialSecondShotsIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", ("shouldGetSecondShot" equ true))
        val potentialFirstShotsIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", ("shouldGetFirstShot" equ true))

        val potentialFirstShots = potentialFirstShotsIterator.toList
        val potentialSecondShots = potentialSecondShotsIterator.toList

        var fs_length = potentialFirstShots.length
        var ss_length = potentialSecondShots.length

        // Clip
        if (ss_length < nSecondShots) {
          wastedSecondShots = nSecondShots - ss_length
          nSecondShots = ss_length
        }

        if (fs_length < nFirstShots) {
          wastedFirstShots = nFirstShots - fs_length
          nFirstShots = fs_length
        }

        nFirstShots = nFirstShots + wastedSecondShots // Swap and add the extras
        nSecondShots = nSecondShots + wastedFirstShots


        // Repeat clipping
        if (ss_length < nSecondShots) {
          wastedSecondShots = nSecondShots - ss_length
          nSecondShots = ss_length
        }
        else {
          wastedSecondShots = 0
        }


        if (fs_length < nFirstShots) {
          wastedFirstShots = nFirstShots - fs_length
          nFirstShots = fs_length
        }
        else {
          wastedFirstShots = 0
        }

        val secondShotsToAdminister = potentialSecondShots.take(nSecondShots)


        secondShotsToAdminister.foreach(node => {
          val person = node.as[Person]

          val gammaMaxFirstShot = person.gamma + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - person.gamma) - person.gamma)
          val gammaMaxSecondShot = gammaMaxFirstShot + (1 - (Disease.vaccinatedGammaFractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - gammaMaxFirstShot) - gammaMaxFirstShot)

          person.updateParam("vaccinationStatus", true)
          person.updateParam("vaccineShots", person.vaccineShots + 1)
          person.updateParam("receivedSecondShotOn", (context.getCurrentStep + 1) * Disease.dt)

          person.updateParam("gammaMaxSecondShot", gammaMaxSecondShot) // Added to increase vaccination effect in time

          ageWiseVaccinesAdministered(floorAge(person.age)) += 1

          vaccinesAdministered = vaccinesAdministered + 1
          vaccinesAdministeredToday = vaccinesAdministeredToday + 1
          vaccinesAvailableToday = vaccinesAvailableToday - 1
        })


        val firstShotsToAdminister = potentialFirstShots.take(nFirstShots)

        firstShotsToAdminister.foreach(node => {
          val person = node.as[Person].asInstanceOf[Person]

          val gammaMaxFirstShot = person.gamma + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - person.gamma) - person.gamma)

          person.updateParam("vaccinationStatus", true)
          person.updateParam("vaccineShots", person.vaccineShots + 1)
          person.updateParam("receivedFirstShotOn", (context.getCurrentStep + 1) * Disease.dt)

          person.updateParam("gammaMaxFirstShot", gammaMaxFirstShot)

          ageWiseVaccinesAdministered(floorAge(person.age)) += 1

          vaccinesAdministered = vaccinesAdministered + 1
          vaccinesAdministeredToday = vaccinesAdministeredToday + 1
          vaccinesAvailableToday = vaccinesAvailableToday - 1
        })

        val totalWastage = wastedFirstShots + wastedSecondShots

        potentialFirstShots.foreach(node => {
          val person = node.as[Person]
          person.updateParam("shouldGetFirstShot", false)
        })

        potentialSecondShots.foreach(node => {
          val person = node.as[Person]
          person.updateParam("shouldGetSecondShot", false)
        })

      }
    }

    val intervention: Intervention = SingleInvocationIntervention(interventionName, activationCondition, deActivationCondition, firstTimeExecution, perTickAction)

    registerIntervention(intervention)
  }

  private def floorAge(age: Int): Int = { //Copied from BharatSim master (21.06.21)
    (age / 10).floor.toInt
  }

  private def lockdown(implicit context: Context): Unit = {

    var ActivatedAt = 0
    val LockdownDurationDays = 15 // Lockdown goes on for 15 days
    val interventionName = "lockdown"
    val activationCondition = (context: Context) => {
      val result = getInfectedCount(context) >= Disease.lockdownTriggerFraction*ingestedPopulation // If there more than the trigger fraction lockdown.
      if (result) {
        lockdownStartedOn = context.getCurrentStep * Disease.dt
        logger.info("Lockdown started on " + lockdownStartedOn)
      }
      result
    }
    val firstTimeExecution = (context: Context) => ActivatedAt = context.getCurrentStep
    val DeactivationCondition = (context: Context) => {
      context.getCurrentStep >= ActivatedAt + (LockdownDurationDays * Disease.inverse_dt) // 15 day lockdown is the default value
    }
    val intervention = SingleInvocationIntervention(interventionName, activationCondition, DeactivationCondition, firstTimeExecution)


    val lockdownSchedule = (Disease.myDay, Disease.myTick).add[Home](0, 1)

    registerIntervention(intervention)
    registerSchedules(
      (lockdownSchedule,
        (agent: Agent, context: Context) => {
          val isEssentialWorker = agent.asInstanceOf[Person].isEssentialWorker
          val violateLockdown = agent.asInstanceOf[Person].violateLockdown // Changed back // Currently, no one violates the lockdown // agent.asInstanceOf[Person].violateLockdown
          val isLockdown = context.activeInterventionNames.contains(interventionName)
          isLockdown && !(isEssentialWorker || violateLockdown)
        },
        2
      )
    )
  }

  private def getInfectedCount(context: Context): Int = {
    val N = context.graphProvider.fetchCount("Person", "age" gte 0)
    val unInfPattern = ("infectionState" equ Susceptible) or ("infectionState" equ Recovered) or ("infectionState" equ Exposed) or ("infectionState" equ Dead)

    N - context.graphProvider.fetchCount("Person", unInfPattern)
  }

  private def printStats(beforeCount: Int)(implicit context: Context): Unit = {
    val afterCountSusceptible = getCount(context, Susceptible)
    val afterCountRecovered = getCount(context, Recovered)

    logger.info("Susceptible: {}", afterCountSusceptible)
    logger.info("Recovered: {}", afterCountRecovered)
    logger.info("Vaccines administered: {}", vaccinesAdministered)

    logger.info("Vaccination Started On: {}", vaccinationStarted)
    logger.info("Lockdown Started On: {}", lockdownStartedOn)
    logger.info("Schools Closed On: {}", schoolsClosedOn)
    logger.info("Schools Opened On: {}", schoolsOpenedOn)

    logger.info("Was infected by Asymptomatic: {}", context.graphProvider.fetchCount("Person", "wasInfectedBy" equ "Asymptomatic"))
    logger.info("Was infected by Presymptomatic: {}", context.graphProvider.fetchCount("Person", "wasInfectedBy" equ "Presymptomatic"))
    logger.info("Was infected by InfectedMild: {}", context.graphProvider.fetchCount("Person", "wasInfectedBy" equ "InfectedMild"))
    logger.info("Was infected by InfectedSevere: {}", context.graphProvider.fetchCount("Person", "wasInfectedBy" equ "InfectedSevere"))
    logger.info("Was infected by Hospitalised: {}", context.graphProvider.fetchCount("Person", "wasInfectedBy" equ "Hospitalised"))
    logger.info("Was infected by FOI: {}", context.graphProvider.fetchCount("Person", "wasInfectedBy" equ "FOI"))

    logger.info("Was infected at Home: {}", context.graphProvider.fetchCount("Person", "wasInfectedAt" equ "Home"))
    logger.info("Was infected at Office: {}", context.graphProvider.fetchCount("Person", "wasInfectedAt" equ "Office"))
    logger.info("Was infected at ClassRoom: {}", context.graphProvider.fetchCount("Person", "wasInfectedAt" equ "ClassRoom"))

  }

  private def getCount(context: Context, infectionStatus: InfectionStatus): Int = {
    context.graphProvider.fetchCount("Person", "infectionState" equ infectionStatus)
  }

}
