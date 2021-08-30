package schools

import com.bharatsim.engine.basicConversions.StringValue
import com.bharatsim.engine.basicConversions.decoders.BasicDecoder
import com.bharatsim.engine.basicConversions.encoders.BasicEncoder

object InfectionStatus extends Enumeration {
  type InfectionStatus = Value
  val Susceptible, Exposed, Asymptomatic, Presymptomatic, InfectedMild, InfectedSevere, Recovered, Hospitalised, Dead = Value

  implicit val infectionStatusDecoder: BasicDecoder[InfectionStatus] = {
    case StringValue(v) => withName(v)
    case _ => throw new RuntimeException("Infection status was not stored as a string")
  }

  implicit val infectionStatusEncoder: BasicEncoder[InfectionStatus] = {
    case Susceptible => StringValue("Susceptible")
    case Exposed => StringValue("Exposed")
    case Asymptomatic => StringValue("Asymptomatic")
    case Presymptomatic => StringValue("Presymptomatic")
    case InfectedMild => StringValue("InfectedMild")
    case InfectedSevere => StringValue("InfectedSevere")
    case Recovered => StringValue("Recovered")
    case Hospitalised => StringValue("Hospitalised")
    case Dead => StringValue("Dead")
  }
}
