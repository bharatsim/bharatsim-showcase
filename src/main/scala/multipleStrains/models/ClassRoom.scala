package multipleStrains.models

import com.bharatsim.engine.models.Network


case class ClassRoom(classRoomId: Int) extends Network {
  override def getContactProbability(): Double = 1

  addRelation[Person]("CLASS_TEACHES")
}
