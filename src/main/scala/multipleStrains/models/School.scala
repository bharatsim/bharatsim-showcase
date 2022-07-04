package multipleStrains.models

import com.bharatsim.engine.models.Network

case class School(schoolId: Long) extends Network {
  override def getContactProbability(): Double = 1

  addRelation[Person]("TEACHES")
}
