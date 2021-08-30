package schools

import com.bharatsim.engine.distributions.LogNormal

import scala.collection.immutable.HashMap


object Disease {

  // Initial Population ***************************//

  final val asymptomaticFraction = 1f / 100
  final val ageStratifiedBetaMultiplier = HashMap(
    9 -> 1.0,
    19 -> 1.0,
    29 -> 1.0,
    39 -> 1.0,
    49 -> 1.0,
    59 -> 1.0,
    69 -> 1.0,
    79 -> 1.0,
    89 -> 1.0,
    99 -> 1.0
  )
  final val ageStratifiedOneMinusGamma = HashMap( // Fraction going from S -> P
    9 -> 0.5,
    19 -> 0.55,
    29 -> 0.6,
    39 -> 0.65,
    49 -> 0.7,
    59 -> 0.75,
    69 -> 0.8,
    79 -> 0.85,
    89 -> 0.9,
    99 -> 0.9
  )
  final val ageStratifiedOneMinusDelta = HashMap( // Fraction going from S -> SI
    9 -> 0.0005,
    19 -> 0.00165,
    29 -> 0.00720,
    39 -> 0.02080,
    49 -> 0.03430,
    59 -> 0.07650,
    69 -> 0.13280,
    79 -> 0.20655,
    89 -> 0.24570,
    99 -> 0.24570
  )
  final val ageStratifiedSigma = HashMap( // Fraction going from H -> D
    9 -> 0.00002,
    19 -> 0.00002,
    29 -> 0.0001,
    39 -> 0.00032,
    49 -> 0.00098,
    59 -> 0.00265,
    69 -> 0.00766,
    79 -> 0.02439,
    89 -> 0.08292,
    99 -> 0.16190
  )

  // **********************************************//


  // Residence Times and Branching Ratios *********//

  //  final val ageStratifiedBetaMultiplier = HashMap( // COVASIM's age-stratified relative risk of infection
  //    9 -> 0.34,
  //    19 -> 0.67,
  //    29 -> 1.0,
  //    39 -> 1.0,
  //    49 -> 1.0,
  //    59 -> 1.0,
  //    69 -> 1.0,
  //    79 -> 1.24,
  //    89 -> 1.47,
  //    99 -> 1.47
  //  )
  final val lambda_A: Double = 0.143
  final val lambda_P: Double = 0.5
  final val lambda_MI: Double = 0.1
  final val lambda_SI: Double = 0.5
  final val lambda_H: Double = 0.1
  final val inverse_dt = 2
  final val dt: Double = 1f / inverse_dt // per 4hour dt
  final val splittableRandom: RandomNumberGenerator = RandomNumberGenerator()
  //  Possibility of Exponential Residence Times
  //  ******************************************
  //  All the samples drawn from these distributions represent days
  final val exposedDurationProbabilityDistribution = LogNormal(4.5, 1.5)
  final val presymptomaticDurationProbabilityDistribution = LogNormal(1.1, 0.9)
  final val asymptomaticDurationProbabilityDistribution = LogNormal(8, 2)
  final val mildSymptomaticDurationProbabilityDistribution = LogNormal(8, 2)
  final val severeSymptomaticDurationProbabilityDistribution = LogNormal(1.5, 2.0)

  //  Possibility of Exponential Residence Times
  //  ******************************************
  //  final val asymptomaticDurationProbabilityDistribution = Exponential(1 / lambda_A)
  //  final val presymptomaticDurationProbabilityDistribution = Exponential(1 / lambda_P)
  //  final val mildSymptomaticDurationProbabilityDistribution = Exponential(1 / lambda_MI)
  //  final val severeSymptomaticDurationProbabilityDistribution = Exponential(1 / lambda_SI)
  //  final val HospitalisedProbabilityDuration = Exponential(1 / lambda_H)
  final val hospitalisedDurationProbabilityDistribution = LogNormal(18.1, 6.3)
  final val tau = 0.8
  final val phase1 = List(69, 79, 89, 99)
  final val phase2 = List(49, 59, 69, 79, 89, 99)
  final val phase3 = List(29, 39, 49, 59, 69, 79, 89, 99)
  final val phase1_endDate = 0 // Changed from 30 (because we're starting with ~80% of the 60+ vaccinated already)


  //**********************************************************************************//


  // Vaccination Phase Details ***********************//
  final val phase2_endDate = 30 // Idem: from 60
  final val Delta = 90
  final val vaccinatedBetaMultiplier_firstShot = 1.0 - 0.272
  final val vaccinatedBetaMultiplier_secondShot = 1.0 - 0.441
  final val vaccinatedGammaFractionalIncrease_firstShot = 1 - 0.335
  final val vaccinatedGammaFractionalIncrease_secondShot = 1 - 0.598
  final val fractionalTransmissionReduction = 0.4
  val nClassrooms = 100
  var recoveredFraction: Float = 30f / 100
  var vaccinatedOneShotFraction: Float = 20f / 100
  var vaccinatedTwoShotFraction: Float = 10f / 100
  var lambda_S: Double = 0.7
  var vaccinationRate: Double = 0.0d / 100d

  // *************************************************//


  // FOI Details *************************************//
  var alpha = 0.0

  def backgroundFOI(t: Double, tpeak: Double = 8.5, width: Double = 10): Double = {
    alpha * 0.07 * math.exp(-math.pow((t - tpeak), 2) / (2 * width))
  }

  // **************************************************//

}
