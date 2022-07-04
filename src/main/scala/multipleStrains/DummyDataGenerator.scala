package multipleStrains

import com.bharatsim.engine.utils.Probability.biasedCoinToss
import com.github.tototoshi.csv.CSVWriter

import scala.annotation.tailrec
import scala.util.Random

object DummyDataGenerator {
  val headers = List(
    "Agent_ID",
    "Age",
    "PublicTransport_Jobs",
    "essential_worker",
    "Adherence_to_Intervention",
    "AdminUnitName",
    "H_Lat",
    "H_Lon",
    "HHID",
    "school_id",
    "WorkPlaceID"
  )

  private val averageEmployeesPerOffice = 30
  
  val totalPopulation = 10_000
  val ESSENTIAL_WORKER_FRACTION = 0.2
  val PUBLIC_TRANSPORT_FRACTION = 0.3
  val totalOffices: Int = (totalPopulation / 2) / averageEmployeesPerOffice

  val TotalTeachers = 300
  val totalSchools = 10
  val totalHospitals = 5 // ToDo : Create a few hospital locations.

  val random = new Random()
  val writer: CSVWriter = CSVWriter.open("src/main/resources/dummy10k.csv")


  // ToDo: Create a teacher job label, assign to classrooms and schools. Check if a teacher has a class assigned.
  
  var teachcount = 0

  def main(): Unit = {
    generate()
  }

  @tailrec
  private def generateRow(rowNum: Int): Unit = {
    val id = rowNum
    val age = random.between(5, 80)
    val houseId = random.between(1, totalPopulation / 4 + 1)
    val isEmployee = age >= 30
    val isTeacher = isEmployee && teachcount < TotalTeachers
    if (isTeacher) {
      teachcount += 1
    }
    val isStudent = !isEmployee
    val officeId = if (isEmployee & !isTeacher) random.between(1, totalOffices + 1) else if (isTeacher) random.between(totalOffices + 1, totalOffices + totalSchools + 1) else 0
    val schoolId = if (isStudent) random.between(totalOffices + 1, totalOffices + totalSchools + 1) else if (isTeacher) officeId else 0 //so school ids are different from office ids
    val publicTransport = if (biasedCoinToss(PUBLIC_TRANSPORT_FRACTION)) 1 else 0
    val isEssentialWorker = if (isEmployee && biasedCoinToss(ESSENTIAL_WORKER_FRACTION)) 1 else 0

    val violatesLockdown: Double = if (biasedCoinToss(0.1)) random.between(0.0, 0.5) else random.between(0.5, 1.0)
    val scale = math pow(10, 1)
    val village_town = "some_village"
    val latitude = Random.nextFloat()
    val longitude = Random.nextFloat()

    writer.writeRow(
      List(
        id,
        age,
        publicTransport,
        isEssentialWorker,
        (math round violatesLockdown * scale) / scale,
        village_town,
        latitude,
        longitude,
        houseId,
        schoolId,
        officeId
      )
    )

    if (rowNum < totalPopulation) {
      generateRow(rowNum + 1)
    }
  }

  private def generate(): Unit = {
    println("Total schools", totalSchools)
    println("Total offices", totalOffices)
    writer.writeRow(headers)
    generateRow(1)
    println("Total teachers", teachcount)

  }
}

