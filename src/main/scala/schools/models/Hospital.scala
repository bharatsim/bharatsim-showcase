package schools.models

import com.bharatsim.engine.models.Network

case class Hospital(hospitalID: Int) extends Network {
  override def getContactProbability(): Double = 0.05

  addRelation[Person]("TREATS")

}