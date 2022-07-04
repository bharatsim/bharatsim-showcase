package multipleStrains

import com.bharatsim.engine.basicConversions.StringValue
import com.bharatsim.engine.basicConversions.decoders.BasicDecoder
import com.bharatsim.engine.basicConversions.encoders.BasicEncoder

object InfectionStatus extends Enumeration {
  type InfectionStatus = Value
  val Susceptible,  Exposed , Asymptomatic , PreSymptomatic , InfectedMild , InfectedSevere , Hospitalised , Recovered , Dead  = Value
  val Susceptible2, Exposed2, Asymptomatic2, PreSymptomatic2, InfectedMild2, InfectedSevere2, Hospitalised2, Recovered2, Dead2 = Value

  implicit val infectionStatusDecoder: BasicDecoder[InfectionStatus] = {
    case StringValue(v) => withName(v)
    case _ => throw new RuntimeException("Infection status was not stored as a string")
  }

  implicit val infectionStatusEncoder: BasicEncoder[InfectionStatus] = {
    case Susceptible => StringValue("Susceptible")
    case Exposed => StringValue("Exposed")
    case Asymptomatic => StringValue("Asymptomatic")
    case PreSymptomatic => StringValue("PreSymptomatic")
    case InfectedMild => StringValue("InfectedMild")
    case InfectedSevere => StringValue("InfectedSevere")
    case Hospitalised => StringValue("Hospitalised")
    case Recovered => StringValue("Recovered")
    case Dead => StringValue("Dead")

    case Susceptible2 => StringValue("Susceptible2")
    case Exposed2 => StringValue("Exposed2")
    case Asymptomatic2 => StringValue("Asymptomatic2")
    case PreSymptomatic2 => StringValue("PreSymptomatic2")
    case InfectedMild2 => StringValue("InfectedMild2")
    case InfectedSevere2 => StringValue("InfectedSevere2")
    case Hospitalised2 => StringValue("Hospitalised2")
    case Recovered2 => StringValue("Recovered2")
    case Dead2 => StringValue("Dead2")
  }

}
