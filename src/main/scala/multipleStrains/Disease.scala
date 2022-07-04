package multipleStrains

import scala.collection.immutable.HashMap
import com.bharatsim.engine.distributions.{LogNormal}

object Disease {

  var beta: Double = 0.7
  var beta2: Double = 0.7*0.7/0.3
//
  final val initialExposedFraction1 = 0.01
  final val initialExposedFraction2 = 0.0

  final val seededExposedFraction1 = 0.00
  final val seededExposedFraction2 = 0.01

  var initialRecoveredFraction1: Float = 00f / 100
  var initialRecoveredFraction2: Float = 00f / 100
//
//  final val severeInfectedFraction = 0.2
//  final val severeInfectedFraction2 = 0.2
//
//  final val deathFraction = 0.02
//  final val deathFraction2 = 0.02

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


////  All the samples drawn from these distributions represent days
//  final val exposedDurationProbabilityDistribution = LogNormal(4.6, 4.8)
//  final val presymptomaticDurationProbabilityDistribution = LogNormal(1, 1)
//  final val asymptomaticDurationProbabilityDistribution = LogNormal(8, 2)
//  final val mildSymptomaticDurationProbabilityDistribution = LogNormal(8, 2)
//  final val severeSymptomaticDurationProbabilityDistribution = LogNormal(14, 2.4)

  final val exposedDurationProbabilityDistribution = Exponential(4.6)
  final val presymptomaticDurationProbabilityDistribution = Exponential(1)
  final val asymptomaticDurationProbabilityDistribution = Exponential(8)
  final val mildSymptomaticDurationProbabilityDistribution = Exponential(8)
  final val severeSymptomaticDurationProbabilityDistribution = Exponential(14)
  final val hospitalisedDurationProbabilityDistribution = Exponential(7)

  final val inverse_dt = 2
  final val dt = 1.toDouble / inverse_dt


  //  Does not have any effect, can be used to model reduced chances of catching an infection due to masking or such other interventions
//  final val betaMultiplier = 1.0
//  final val fractionalTransmissionReduction = 0.2

  //    TODO: Add the change to master - Jayanta / Philip
  var vaccinationRate = 0.05
//  final val vaccinatedGammaFractionalIncrease = 2.0

   // Vaccination Phase Details ***********************//
   final val tau = 0.8
   final val phase1 = List(69, 79, 89, 99)
   final val phase2 = List(49, 59, 69, 79, 89, 99)
   final val phase3 = List(29, 39, 49, 59, 69, 79, 89, 99)
   final val phase1_endDate = 0 // Changed from 30 (because we're starting with ~80% of the 60+ vaccinated already)

  final val phase2_endDate = 30 // Idem: from 60
  final val Delta = 90
   final val vaccinatedBetaMultiplier_firstShot = 1.0 - 0.272
   final val vaccinatedBetaMultiplier_secondShot = 1.0 - 0.441
   final val vaccinatedGammaFractionalIncrease_firstShot = 1 - 0.335
   final val vaccinatedGammaFractionalIncrease_secondShot = 1 - 0.598
   final val fractionalTransmissionReduction = 0.4
   val nClassrooms = 100

   var vaccinatedOneShotFraction: Float = 20f / 100
   var vaccinatedTwoShotFraction: Float = 10f / 100





}