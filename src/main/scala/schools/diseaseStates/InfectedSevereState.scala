package schools.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import schools.{Disease, Main}
import schools.InfectionStatus.InfectedSevere
import schools.models.Person

case class InfectedSevereState(time: Double, toBeH: Boolean) extends State { // toBeH is currently always true (all SI go to H in IndSciSim)

  var agentSigma: Double = 0

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState", InfectedSevere)
  }

  override def perTickAction(context: Context, agent: StatefulAgent): Unit = {
    agentSigma = agent.asInstanceOf[Person].sigma
  }


  private def shouldMoveToH(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && toBeH) {
      return true
    }
    false
  }

  private def shouldMoveToR(context: Context, agent: StatefulAgent): Boolean = {
    if (context.getCurrentStep >= time && !toBeH) {
      return true
    }
    false
  }


  addTransition(
    when = shouldMoveToH,
    to = context => HospitalisedState(context.getCurrentStep + Disease.inverse_dt * Disease.hospitalisedDurationProbabilityDistribution.sample(), Main.splittableRandom.nextDouble() < agentSigma)
  )

  addTransition(
    when = shouldMoveToR,
    to = context => RecoveredState()
  )

}
