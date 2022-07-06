package multipleStrains

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
import com.bharatsim.engine.intervention.{Intervention, OffsetBasedIntervention, SingleInvocationIntervention}
import com.bharatsim.engine.listeners.{CsvOutputGenerator, SimulationListenerRegistry}
import com.bharatsim.engine.models.Agent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import com.typesafe.scalalogging.LazyLogging
import multipleStrains.InfectionStatus._
import multipleStrains.models._

object Main extends LazyLogging {

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


  def main(args: Array[String]): Unit = {
    var beforeCount = 0
    val simulation = Simulation()

    logger.info("Bare Beta1 = " + Disease.beta)
    logger.info("Bare Beta2 = " + Disease.beta2)
    logger.info("Arguments provided: ")
    args.foreach(print)

    if (args.length != 0) {
      logger.info("Accepting arguments")

      // Input and output csv file paths
      Disease.inputPath = args(0)
      Disease.outputPath = args(1)

      // Initial Recovered Parameters
      Disease.initialRecoveredFraction1 = args(2).toFloat / 100

      // Initial Vaccinated Parameters
      Disease.prevaccinate = args(3) == "prevaccinate"
      Disease.prevaccinateFamilies = args(4) == "families"
      Disease.vaccinatedOneShotFraction = args(5).toFloat / 100
      Disease.vaccinatedTwoShotFraction = args(6).toFloat / 100

      // Rolling Vaccination Parameters
      Disease.vaccinatePeople = args(7) == "rollingVaccinations"
      Disease.vaccinationRate = args(8).toDouble / 100d

      // Close Schools
      Disease.closeSchools = args(9) == "closeSchools"
      Disease.unlockSchoolsAt = args(10).toInt

      // Lockdown
      Disease.lockdownEveryone = args(11) == "lockdown"
      Disease.lockdownTriggerFraction = args(12).toFloat / 100 // also for vaccines TODO: use this somewhere

      // Relative risks
      Disease.reinfectionRisk = args(13).toDouble
    }

    logger.info("Phase1 " + Disease.phase1 + " ends on day " + Disease.phase1_endDate)
    logger.info("Phase2 " + Disease.phase2 + " ends on day " + Disease.phase2_endDate)

    simulation.ingestData { implicit context =>
            ingestCSVData(Disease.inputPath, mapper)
      logger.debug("Ingestion done")
    }


    simulation.defineSimulation { implicit context =>
      ingestedPopulation = context.graphProvider.fetchCount("Person", EmptyPattern())

      if (Disease.prevaccinate) {
        prevaccination(shot = 1)
        prevaccination(shot = 2)
      }

      if (Disease.vaccinatePeople) {
        vaccination
      }

      closeSchoolsUntil

      if (Disease.lockdownEveryone) {
        lockdown
      }

      create12HourSchedules()
      introductionOfStrain2()

      registerAction(
        StopSimulation,
        (c: Context) => {
          c.getCurrentStep >= (300 * inverse_dt)
        }
      )

      beforeCount = getInfectedCount(context)

      registerAgent[Person]

      val rn = splittableRandom.nextDouble()
      val closeschoolslabel = if (Disease.unlockSchoolsAt > 0) 1 else 0
      val lockdownlabel = if (Disease.lockdownEveryone) 1 else 0

      var label = "_IR_" + (Disease.initialRecoveredFraction1 * 100).toInt + "_IV_" + (Disease.vaccinatedOneShotFraction * 100).toInt + "_" + (Disease.vaccinatedTwoShotFraction * 100).toInt
      label += "_VR_" + (Disease.vaccinationRate * 100)
      label += "_closeSchools_" + closeschoolslabel + "_unlockSchoolsAt_" + Disease.unlockSchoolsAt
      label += "_lockdown_" + lockdownlabel

      label += "_reinfectionRisk_"+Disease.reinfectionRisk

      SimulationListenerRegistry.register(
        new CsvOutputGenerator(Disease.outputPath+"total_output" + label + "_" + rn + ".csv", new SIROutput(context))
      )
//      SimulationListenerRegistry.register(
//        new CsvOutputGenerator(outputPath+"/agewise_output" + label + "_" + rn + ".csv", new SIRAgewiseOutput(context))
//      )
//      SimulationListenerRegistry.register(
//        new CsvOutputGenerator(outputPath+"/infectioninfo_output" + label + "_" + rn + ".csv", new InfectionInfoOutput(context))
//      )
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
    val infectionStates: (String,String) = setInitialPopulation()
    val infectionState1 = infectionStates._1
    val infectionState2 = infectionStates._2
    val isEssentialWorker = row("essential_worker").toInt == 1
    val violateLockdown = row("Adherence_to_Intervention").toFloat < 0.5

    val villageTown = row("AdminUnitName")
    val lat = row("H_Lat")
    val long = row("H_Lon")
    val homeId = row("HHID").toLong
    val home = Home(homeId)
    val data = GraphData()
    val staysAt = Relation[Person, Home](agentID, "STAYS_AT", homeId)
    val houses = Relation[Home, Person](homeId, "HOUSES", agentID)

    val schoolID = row("school_id").toLong
    val officeID = row("WorkPlaceID").toLong / 100 // ToDo: Fix the synthetic population so that we don't need to do this
    // ToDo: Perhaps add an FOI?

    val isEmployee: Boolean = officeID > 0
    val isTeacher: Boolean = officeID > 0 && schoolID > 0 && officeID == schoolID
    val isStudent: Boolean = schoolID > 0 && !isTeacher
    val isHomebound: Boolean = officeID == 0 && schoolID == 0

    if (!isEmployee && !isTeacher && !isStudent && !isHomebound) {
      print("Agent " + agentID)
    }

    // Prevaccinate 80% of the 60+, and use the remaining to prevaccinate the population randomly
    val prevaccinate = age > 60 && splittableRandom.nextDouble() < 0.8

    // Alternatively, one could prevaccinate family members and teachers
    //    val prevaccinate = (row("fam_mem").toInt == 1 || isTeacher) && prevaccinateFamilies

    val exitTime1=
      if (infectionState1 == "Exposed")
        Disease.exposedDurationProbabilityDistribution.sample() * Disease.inverse_dt
      else 0.0

    val exitTime2 =
      if (infectionState2 == "Exposed2")
        Disease.exposedDurationProbabilityDistribution.sample() * Disease.inverse_dt
      else 0.0


    val agent: Person = Person(
      agentID,
      age,
      InfectionStatus.withName(infectionState1),
      InfectionStatus.withName(infectionState2),
      isEssentialWorker,
      violateLockdown,
      villageTown,
      lat,
      long,
      isEmployee,
      isStudent,
      isTeacher,
      isHomebound,
      gamma1 = 1.0 - Disease.ageStratifiedOneMinusGamma.getOrElse(roundToAgeRange(age), Disease.ageStratifiedOneMinusGamma(99)),
      gamma2 = 1.0 - Disease.ageStratifiedOneMinusGamma.getOrElse(roundToAgeRange(age), Disease.ageStratifiedOneMinusGamma(99)),
      betaMultiplier   = 1,
      gammaMultiplier  = 1,
      gammaMultiplier2 = 1,
      exitTime  = exitTime1,
      exitTime2 = exitTime2,
      prevaccinate = prevaccinate
    )

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


    if ((infectionState1 == Hospitalised.toString) || (infectionState2 == Hospitalised2.toString) ) {
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


  private def setInitialPopulation(): (String,String) = {
    var x = splittableRandom.nextDouble()
    var infectionState1 = ""
    if (x <= Disease.initialExposedFraction1) {
      infectionState1 = "Exposed"
    }
    else if (Disease.initialExposedFraction1 < x && x <= (Disease.initialRecoveredFraction1 + Disease.initialExposedFraction1)) {
      infectionState1 = "Recovered"
    }
    else {
      infectionState1 = "Susceptible"
    }

    x = splittableRandom.nextDouble()
    var infectionState2 = ""
    if (x <= Disease.initialExposedFraction2) {
      infectionState2 = "Exposed2"
    }
    else if (Disease.initialExposedFraction2 < x && x <= (Disease.initialRecoveredFraction2 + Disease.initialExposedFraction2)) {
      infectionState2 = "Recovered2"
    }
    else {
      infectionState2 = "Susceptible2"
    }

    (infectionState1, infectionState2)
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
      val currentlyUnvaccinated = context.graphProvider.fetchCount("Person", "vaccinationStatus" equ false).toDouble
      val result = context.getCurrentStep >= Disease.unlockSchoolsAt * Disease.inverse_dt
      if (result) {
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

  private def prevaccinateOne(person: Person, shot: Int)(implicit context: Context): Unit = {
    val agentGamma1 = person.gamma1
    val gamma1MaxFirstShot = agentGamma1 + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - agentGamma1) - agentGamma1)
    val gamma1MaxSecondShot = gamma1MaxFirstShot + (1 - (Disease.vaccinatedGammaFractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - gamma1MaxFirstShot) - gamma1MaxFirstShot)

    val agentGamma2 = person.gamma2
    val gamma2MaxFirstShot = agentGamma2 + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - agentGamma2) - agentGamma2)
    val gamma2MaxSecondShot = gamma2MaxFirstShot + (1 - (Disease.vaccinatedGammaFractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - gamma2MaxFirstShot) - gamma2MaxFirstShot)

    person.updateParam("vaccinationStatus", true)
    if (shot == 1) {
      person.updateParam("vaccineShots", 1)
      person.updateParam("receivedFirstShotOn", -10000.0)

      person.updateParam("gamma1MaxFirstShot", gamma1MaxFirstShot)
      person.updateParam("gamma2MaxFirstShot", gamma2MaxFirstShot)

      ageWiseVaccinesAdministered(floorAge(person.age)) += 1
      vaccinesAdministered = vaccinesAdministered + 1
    }
    else if (shot == 2) {
      person.updateParam("vaccineShots", 2)
      person.updateParam("receivedFirstShotOn", -10000.0)
      person.updateParam("receivedSecondShotOn", -10000.0)

      person.updateParam("gamma1MaxFirstShot", gamma1MaxFirstShot)
      person.updateParam("gamma1MaxSecondShot", gamma1MaxSecondShot)

      person.updateParam("gamma2MaxFirstShot", gamma2MaxFirstShot)
      person.updateParam("gamma2MaxSecondShot", gamma2MaxSecondShot)

      ageWiseVaccinesAdministered(floorAge(person.age)) += 1
      vaccinesAdministered = vaccinesAdministered + 1
    }

  }

  private def prevaccination(shot: Int)(implicit context: Context): Unit = {

    val preVaccinesAvailable = if (shot == 1) (Disease.vaccinatedOneShotFraction * ingestedPopulation).toInt else if (shot == 2) (Disease.vaccinatedTwoShotFraction * ingestedPopulation).toInt else 0

    val preVaccinationFamilyIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", "prevaccinate" equ true)

    val preVaccinationFamilyList = preVaccinationFamilyIterator.toList.take(preVaccinesAvailable)

    val family_prevaccinations = preVaccinationFamilyList.length
    var random_prevaccinations = 0

    val remaining_vaccines = preVaccinesAvailable - family_prevaccinations


    preVaccinationFamilyList.foreach(node => {
      val person = node.as[Person]
      prevaccinateOne(person,shot)
    })

    if (remaining_vaccines > 0) {

      val preVaccinationRandomIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", ("prevaccinate" equ false) and ("age" gte 18))
      val preVaccinationRandomList = preVaccinationRandomIterator.toList.take(remaining_vaccines)

      random_prevaccinations = remaining_vaccines

      preVaccinationRandomList.foreach(node => {
        val person = node.as[Person]
        prevaccinateOne(person, shot)
      })
    }

    logger.info("Prevaccinations done for shot " + shot + ": " + preVaccinesAvailable + " Prioritised: " + family_prevaccinations + " Random: " + random_prevaccinations + " Total: " + (family_prevaccinations + random_prevaccinations))

  }

  private def vaccinateOne(person: Person, shot: Int)(implicit context: Context): Unit = {
    val agentGamma1 = person.gamma1
    val gamma1MaxFirstShot = agentGamma1 + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - agentGamma1) - agentGamma1)
    val gamma1MaxSecondShot = gamma1MaxFirstShot + (1 - (Disease.vaccinatedGammaFractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - gamma1MaxFirstShot) - gamma1MaxFirstShot)

    val agentGamma2 = person.gamma2
    val gamma2MaxFirstShot = agentGamma2 + (1 - (Disease.vaccinatedGammaFractionalIncrease_firstShot / Disease.vaccinatedBetaMultiplier_firstShot) * (1 - agentGamma2) - agentGamma2)
    val gamma2MaxSecondShot = gamma2MaxFirstShot + (1 - (Disease.vaccinatedGammaFractionalIncrease_secondShot / Disease.vaccinatedBetaMultiplier_secondShot) * (1 - gamma2MaxFirstShot) - gamma2MaxFirstShot)

    person.updateParam("vaccinationStatus", true)
    person.updateParam("vaccineShots", person.vaccineShots + 1)

    if(shot == 1){
      person.updateParam("receivedFirstShotOn", (context.getCurrentStep + 1) * Disease.dt)

      person.updateParam("gamma1MaxFirstShot", gamma1MaxFirstShot) // Added to increase vaccination effect in time
      person.updateParam("gamma2MaxFirstShot", gamma2MaxFirstShot) // Added to increase vaccination effect in time

    }
    else if(shot == 2){
      person.updateParam("receivedSecondShotOn", (context.getCurrentStep + 1) * Disease.dt)

      person.updateParam("gamma1MaxSecondShot", gamma1MaxSecondShot) // Added to increase vaccination effect in time
      person.updateParam("gamma2MaxSecondShot", gamma2MaxSecondShot) // Added to increase vaccination effect in time
    }

  }

  private def vaccination(implicit context: Context): Unit = {

    var ActivatedAt = 0
    val interventionName = "vaccination"
    val activationCondition = (context: Context) => {
      val conditionMet = context.getCurrentStep >= 0
      if (conditionMet) {
        vaccinationStarted = context.getCurrentStep
        logger.info("Vaccination started on day "+vaccinationStarted*Disease.dt)
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

        val potentialSecondShotsIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", "shouldGetSecondShot" equ true)
        val potentialFirstShotsIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", "shouldGetFirstShot" equ true)

        val potentialFirstShots = potentialFirstShotsIterator.toList
        val potentialSecondShots = potentialSecondShotsIterator.toList

        val fs_length = potentialFirstShots.length
        val ss_length = potentialSecondShots.length

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

          vaccinateOne(person, shot=2)(context)

          ageWiseVaccinesAdministered(floorAge(person.age)) += 1

          vaccinesAdministered = vaccinesAdministered + 1
          vaccinesAdministeredToday = vaccinesAdministeredToday + 1
          vaccinesAvailableToday = vaccinesAvailableToday - 1
        })


        val firstShotsToAdminister = potentialFirstShots.take(nFirstShots)

        firstShotsToAdminister.foreach(node => {
          val person = node.as[Person]

          vaccinateOne(person, shot=1)(context)

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

  private def introductionOfStrain2()(implicit context: Context): Unit = {
    val interventionName = "strain2"

    val activationCondition = (context: Context) => {
      context.getCurrentStep * Disease.dt == Disease.secondStrainSeededOn
    }

    val firstTimeExecution = (context: Context) => {
      val populationIterable: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person")

      populationIterable.foreach(node => {
        val person = node.as[Person]
        if (biasedCoinToss(Disease.seededExposedFraction2)) {
          val exitTime2 = context.getCurrentStep + Disease.exposedDurationProbabilityDistribution.sample() * Disease.inverse_dt
          person.updateParam("infectionState2", "Exposed2")
          person.updateParam("exitTime2", exitTime2)
        }

        if (biasedCoinToss(Disease.seededExposedFraction1)) {
          val exitTime1 = context.getCurrentStep + Disease.exposedDurationProbabilityDistribution.sample() * Disease.inverse_dt
          person.updateParam("infectionState", "Exposed")
          person.updateParam("exitTime2", exitTime1)
        }
      })
    }

    val intervention: Intervention = OffsetBasedIntervention(interventionName, activationCondition, 1, firstTimeExecution)

    registerIntervention(intervention)
  }


  private def lockdown(implicit context: Context): Unit = {

    var ActivatedAt = 0
    val LockdownDurationDays = 1000 // Lockdown goes on forever
    val interventionName = "lockdown"
    val activationCondition = (context: Context) => {
      val result = getInfectedCount(context) >= 0 // If there are any infected at all, lockdown.
      if (result) {
        lockdownStartedOn = context.getCurrentStep * Disease.dt
        logger.info("Lockdown started on " + lockdownStartedOn)
      }
      result
    }
    val firstTimeExecution = (context: Context) => ActivatedAt = context.getCurrentStep
    val DeactivationCondition = (context: Context) => {
      context.getCurrentStep >= ActivatedAt + (LockdownDurationDays * Disease.inverse_dt) // FOREVER lockdown is the default value
    }
    val intervention = SingleInvocationIntervention(interventionName, activationCondition, DeactivationCondition, firstTimeExecution)


    val lockdownSchedule = (Disease.myDay, Disease.myTick).add[Home](0, 1)

    registerIntervention(intervention)
    registerSchedules(
      (lockdownSchedule,
        (agent: Agent, context: Context) => {
          val isEssentialWorker = agent.asInstanceOf[Person].isEssentialWorker
          val violateLockdown = false // Currently, no one violates the lockdown // agent.asInstanceOf[Person].violateLockdown
          val isLockdown = context.activeInterventionNames.contains(interventionName)
          isLockdown && !(isEssentialWorker || violateLockdown)
        },
        2
      )
    )
  }

  private def getInfectedCount(context: Context): Int = {
    val N = context.graphProvider.fetchCount("Person", "age" gte 0)
    val unInfPattern = ("infectionState" equ Susceptible) or ("infectionState" equ Recovered) or ("infectionState" equ Exposed) or ("infectionState" equ Dead) or ("infectionState2" equ Susceptible2) or ("infectionState2" equ Recovered2) or ("infectionState2" equ Exposed2) or ("infectionState2" equ Dead2)

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

  private def roundToAgeRange(age: Int): Int = { //Copied from BharatSim master (21.06.21)
    (age / 10) * 10 + 9
  }

}
