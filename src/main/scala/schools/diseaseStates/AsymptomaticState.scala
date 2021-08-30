package schools.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import schools.InfectionStatus.Asymptomatic

case class AsymptomaticState(time: Double) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState", Asymptomatic)
  }


  private def shouldMoveToRecovered(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time) {
      return true
    }
    false
  }


  addTransition(
    when = shouldMoveToRecovered,
    to = context => RecoveredState()
  )

}
