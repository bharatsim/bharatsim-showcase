package schools.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import schools.InfectionStatus.Dead

case class DeadState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState", Dead)
  }

}
