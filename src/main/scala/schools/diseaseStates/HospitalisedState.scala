package schools.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import schools.InfectionStatus.Hospitalised

case class HospitalisedState(time: Double, toBeD: Boolean) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState", Hospitalised)
  }


  private def shouldMoveToRecovered(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && !toBeD) {
      return true
    }
    false
  }

  private def shouldMoveToDead(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && toBeD) {
      return true
    }
    false
  }

  addTransition(
    when = shouldMoveToRecovered,
    to = context => RecoveredState()
  )

  addTransition(
    when = shouldMoveToDead,
    to = context => DeadState()
  )

}
