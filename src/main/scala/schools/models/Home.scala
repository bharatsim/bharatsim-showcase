package schools.models

import com.bharatsim.engine.models.Network

case class Home(homeId: Long, gridBox: (Int, Int)) extends Network {
  override def getContactProbability(): Double = 1

  addRelation[Person]("HOUSES")
}
