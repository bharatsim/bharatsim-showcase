package multipleStrains.models

import com.bharatsim.engine.models.Network

case class Office(officeId: Long) extends Network {
  override def getContactProbability(): Double = 1

  addRelation[Person]("EMPLOYER_OF")
}
