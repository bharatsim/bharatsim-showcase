package schools.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import schools.Disease
import schools.InfectionStatus.Exposed
import schools.models.Person

case class ExposedState(time: Double, toBeA: Boolean) extends State {

  var agentDelta: Double = 0

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState", Exposed)
  }

  override def perTickAction(context: Context, agent: StatefulAgent): Unit = {
    agentDelta = agent.asInstanceOf[Person].delta
  }

  private def exitToA(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && toBeA) {
      return true
    }
    false
  }

  private def exitToP(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && !toBeA) {
      return true
    }
    false
  }


  addTransition(
    when = exitToA,
    to = context => AsymptomaticState(context.getCurrentStep + Disease.inverse_dt * Disease.asymptomaticDurationProbabilityDistribution.sample())
  )
  addTransition(
    when = exitToP,
    to = context => PresymptomaticState(context.getCurrentStep + Disease.inverse_dt * Disease.presymptomaticDurationProbabilityDistribution.sample(), Disease.splittableRandom.nextDouble() < agentDelta)
  )
}
