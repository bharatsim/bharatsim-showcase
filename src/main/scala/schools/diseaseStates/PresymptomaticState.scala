package schools.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import schools.Disease
import schools.InfectionStatus.Presymptomatic

case class PresymptomaticState(time: Double, toBeMI: Boolean) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState", Presymptomatic)
  }

  private def shouldMoveToMI(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && toBeMI) {
      return true
    }
    false
  }

  private def shouldMoveToSI(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && !toBeMI) {
      return true
    }
    false
  }

  addTransition(
    when = shouldMoveToMI,
    to = context => InfectedMildState(context.getCurrentStep + Disease.inverse_dt * Disease.mildSymptomaticDurationProbabilityDistribution.sample())
  )

  addTransition(
    when = shouldMoveToSI,
    to = context => InfectedSevereState(context.getCurrentStep + Disease.inverse_dt * Disease.severeSymptomaticDurationProbabilityDistribution.sample(), toBeH = true) // toBeH is always true (all SI go to H in IndSciSim)
  )

}
