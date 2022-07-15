package schools

import com.bharatsim.engine.ScheduleUnit
import com.bharatsim.engine.distributions.LogNormal

import scala.collection.immutable.HashMap


object Disease {

  // Simulation parameters (not disease related) ******************* //

  // Time step parameters:

  final val inverse_dt = 2
  final val dt: Double = 1f / inverse_dt // per 12 hour dt

  final val myTick: ScheduleUnit = new ScheduleUnit(1)
  final val myDay: ScheduleUnit = new ScheduleUnit(myTick * inverse_dt)

  // Default values of arguments:

  var inputPath: String = "Pune_20k_school_population.csv"
  var outputPath: String = "./"

  var unlockSchoolsAt: Int = 0

  var prevaccinateFamilies: Boolean = false

  var vaccinationTriggerFraction: Float = 101f / 100 //TODO: Check default values

  var lockdownTriggerFraction: Float = 101f / 100 //TODO: Check default values
  var lockdownAdherenceThreshold: Float = 75f / 100

  var rampUpBeta = false



  // Initial Population ***************************//

  final val initialExposedFraction = 0.1f / 100

  var localizedWardInfections: Boolean = false
  final val initialInfectedWard = "Baner_Balevadi-Pashan"
  
  var localizedHouseListInfections: Boolean = false
  final val initialInfectedHouseholds: Array[Long] = Array(99801760930L,99800418325L,99800892100L,99801468727L,99801335876L,99801698315L,99800375101L,99801091336L,99801663924L,99801334445L,99800877121L,99800894256L,99801724956L,99801132424L,99801273964L,99800110280L,99801268941L,99801279627L,99801518131L,99801271315L,99801669724L,99800541810L,99800147597L,99800346366L,99801388823L,99800330589L,99800892815L,99801531995L,99801146742L,99801082081L,99801000808L,99801625462L,99800378412L,99801584600L,99800256697L,99800636003L,99801239173L,99801740653L,99801516867L,99801226635L,99801317146L,99800767897L,99801406789L,99801639300L,99800691847L,99800559335L,99800059419L,99800355754L,99801702133L,99800558027L,99800639488L,99801719115L,99801296735L,99801759460L,99800855312L,99800423202L,99801403975L,99800186384L,99800680249L,99801751215L,99801471578L,99801613269L,99801179920L,99800886795L,99801676541L,99801051530L,99801369116L,99801316004L,99800889172L,99801028576L,99801754006L,99800985076L,99801188357L,99800665908L,99801639128L,99800295696L,99801704064L,99801715836L,99800231413L,99801268832L,99800200088L,99801583956L,99800322687L,99800137069L,99801331754L,99801476367L,99800560579L,99801638711L,99801343716L,99801117791L,99800269243L,99800973375L,99800250682L,99801744757L,99801529260L,99800884902L,99800345231L,99800936476L,99800577850L,99801726820L,99800597443L,99801278377L,99801622985L,99801654214L,99801725421L,99801286110L,99800830045L,99801216943L,99801747231L,99800911306L,99800129066L,99800058587L,99801602687L,99801703154L,99801135870L,99800002924L,99801297547L,99800325339L,99801695206L,99800642353L,99800401512L,99800825187L,99800990388L,99801491461L,99800816427L,99800181982L,99800425440L,99801692740L,99800249278L,99800882394L,99801167665L,99800941489L,99800953913L,99801253410L,99801742661L,99801658653L,99800639440L,99801396243L,99800064715L,99800467200L,99800638233L,99800398031L,99801696145L,99801282935L,99800624952L,99801688861L,99800816529L,99800545457L,99801327461L,99800848140L,99801690398L,99800061148L,99800966351L,99800716251L,99800062079L,99801131228L,99801475353L,99800193463L,99801658910L,99801774402L,99801266470L,99800735036L,99800274213L,99801419417L,99800666057L,99801287395L,99801671030L,99800637321L,99800849046L,99801539447L,99800750778L,99800025198L,99801442236L,99801336118L,99800963517L,99801095686L,99800680098L,99801253300L,99800781157L,99800679737L,99801306686L,99801228818L,99800657651L,99801305459L,99801081441L,99800386176L,99800882500L,99800876334L,99800990145L,99800060844L,99801044780L,99801238835L,99800748386L,99800745477L,99800978483L,99801754599L,99800584851L,99801463624L,99801397191L,99800911330L,99801435543L,99800120017L,99801236438L,99801734109L,99801230011L,99801373243L,99801244589L,99800486736L,99801168365L,99801012942L,99801191209L,99800376714L,99800571004L,99800138115L,99801687945L,99800919030L,99800181141L,99800350445L,99801249746L,99800947967L,99801735781L,99800800184L,99801721568L,99801495768L,99800422187L,99800747317L,99801451261L,99801612746L,99801257508L,99801751770L,99800748977L,99801247516L,99801338047L,99801373616L,99801542024L,99801161254L,99800535670L,99800443205L,99801607617L,99801757159L,99800206458L,99801727325L,99801668330L,99801219452L,99801706728L,99801664192L,99800128798L,99801267988L,99801230759L,99801086445L,99800985369L,99801117072L,99801088274L,99800680267L,99801718473L,99801623131L,99800147300L,99800820443L,99800863563L,99800151937L,99801696872L,99801169177L,99801724823L,99801040345L,99800919636L,99800148963L,99800648129L,99800516949L,99801732490L,99800797385L,99800515577L,99801379068L,99800331466L,99801276720L,99800946806L,99800504516L,99801727565L,99801618600L,99800405510L,99801675139L,99800353850L,99801209115L,99801301414L,99801182204L,99800419722L,99800412558L,99801695912L,99800741806L,99801302374L,99801021722L,99800932957L,99801091822L,99800454608L,99800404426L,99801503376L,99801583745L,99800735189L,99801318895L,99801090971L,99800297709L,99801666617L,99801101776L,99801232081L,99801235415L,99800362622L,99801743713L,99800871465L,99800017093L,99800648254L,99800289959L,99801428182L,99801239029L,99800678757L,99801430535L,99800862701L,99801657005L,99800920981L,99801095599L,99800385978L,99801760413L,99800723727L,99800959693L,99800929332L,99800311296L,99801511705L,99801662402L,99801238550L,99800307996L,99800832948L,99801017828L,99800345960L,99800927204L,99801775490L,99800207936L,99801142336L,99800894428L,99801486259L,99801244170L,99801217750L,99801338528L,99801415391L,99801630933L,99801001574L,99801665752L,99800686863L,99801680146L,99800549851L,99801080556L,99801486947L,99800189921L,99800469617L,99800678504L,99800945523L,99801704112L,99800420106L,99800987269L,99801637373L,99800354699L,99800686857L,99800350150L,99800754245L,99801771331L,99800999263L,99801274529L,99800981580L,99801504797L,99800007083L,99801734209L,99801229904L,99801094765L,99800509433L,99800642524L,99801062027L,99801433201L,99801602284L,99800504316L,99801525740L,99801704450L,99800680854L,99800792955L,99801313720L,99801175570L,99800819645L,99801206101L,99801078467L,99800597672L,99801447917L,99801231931L,99801740759L,99801407645L,99800859006L,99800814747L,99800227422L,99801691874L,99801683537L,99800207760L,99801218754L,99800480427L,99801418364L,99801618807L,99801695365L,99800744713L,99801024797L,99800521298L,99800132802L,99801436879L,99800133423L,99800641011L,99800006900L,99801143640L,99801216988L,99801152404L,99800448432L,99801755191L,99801157068L,99801654547L,99800790737L,99800440950L,99801779727L,99800946734L,99800580315L,99801261112L,99801302154L,99800906399L,99800809194L,99801437772L,99801672279L,99801601509L,99800893039L,99801009295L,99801464240L,99800964066L,99800327069L,99801234725L,99801188915L,99800815096L,99800260729L,99800894342L,99800905016L,99801393193L,99801235077L,99801314706L,99801764711L,99801027712L,99800916992L,99800688280L,99801471708L,99801204002L,99801301137L,99800887214L,99800069013L,99801536138L,99801764471L,99801690781L,99800558665L,99801012304L,99801085419L,99800165205L,99801720774L,99800545324L,99800125541L,99801737366L,99801031823L,99800289303L,99800133914L,99800098682L,99801778198L,99800131181L,99801721085L,99801256379L,99801689719L,99801172416L,99800776187L,99800554981L,99801267373L,99801330805L,99800990440L,99800049720L,99801422829L,99801676176L,99800588197L,99800793809L,99801333845L,99801595963L,99800792089L,99800583310L,99801138948L,99800252857L,99800449412L,99800221605L,99800122856L,99801232667L,99800305383L,99801695297L,99800308705L,99801447643L,99801447588L,99801245926L,99801266852L,99801486023L,99800539645L,99801492020L,99801296994L,99800440964L,99800545015L,99801174997L,99801694801L,99800465166L,99800400339L,99800409663L,99800815109L,99800944471L,99801238499L,99801644456L,99800083903L,99800885541L,99801781350L,99801643294L,99801779698L,99801342440L,99800849312L,99800298107L,99801746749L,99801748437L,99800901318L,99801705273L,99800812467L,99800063813L,99801393340L,99800843910L,99800161024L,99801556930L,99800705451L,99800722801L,99801334333L,99801512320L,99801629666L,99800483875L,99800082353L,99801702076L,99800101232L,99800002101L,99800873174L,99800915998L,99801344138L,99800465677L,99800415976L,99801193740L,99801765454L,99800853538L,99800699757L,99800135674L,99800184684L,99801293504L,99801416952L,99800612764L,99801080047L,99801152272L,99800200973L,99801561266L,99800036192L,99800007193L,99801259703L,99801553344L,99801745950L,99801595505L,99801280937L,99800603268L,99800204931L,99801755884L,99801331570L,99800774673L,99801657196L,99800925372L,99801749275L,99801157907L,99800122289L,99801739338L,99801671586L,99801674615L,99801198644L,99801225722L,99801736719L,99801660549L,99801534736L,99800459964L,99800608292L,99800063363L,99801130371L,99800261237L,99801715943L,99801595087L,99801188188L,99800947571L,99800484979L,99801308929L,99801622190L,99801701165L,99800740111L,99800883511L,99801583633L,99800771980L,99800316360L,99801735310L,99800193616L,99800528014L,99801703620L,99800489944L,99800257515L,99801672699L,99801770744L,99800125121L,99801316590L,99800284632L,99801217921L,99801721060L,99800090058L,99801160587L,99800173771L,99800803613L,99800289379L,99800269551L,99800959081L,99800389760L,99801459348L,99800292577L,99801325108L,99801043863L,99800606666L,99800157912L,99801711428L,99800945037L,99801366588L,99800114253L,99800935851L,99801780117L,99801492093L,99800947681L,99801477878L,99801176140L,99801730766L,99801276892L,99801171168L,99801329373L,99800145811L,99801082934L,99801476899L,99801589415L,99801256171L,99800241459L,99801665925L,99800133577L,99800708723L,99800389738L,99800924000L,99800219390L,99801093337L,99800488720L,99800119658L,99801743391L,99800504281L,99800021108L,99800431605L,99801303211L,99800484602L,99801489424L,99801753110L,99801733280L,99801217603L,99801320732L,99801291640L,99801662267L,99801385801L,99800987379L,99801334798L,99801496315L,99801308402L,99800769908L,99800118431L,99800626879L,99800434183L,99801605252L,99800566622L,99800613656L,99800829213L,99800348077L,99800186424L,99801076348L,99800329044L,99801090699L,99801541613L,99800888574L,99801263467L,99801323822L,99800332636L,99801319648L,99801760101L,99800670692L,99801378727L,99801671330L,99801308378L,99801729064L,99800068127L,99801512377L,99801051693L,99800531688L,99800564845L,99801076944L,99801443181L,99801334564L,99800096234L,99800455235L,99801001501L,99801747132L,99801713808L,99800767924L,99801701542L,99800103922L,99801668315L,99801541430L,99801556136L,99800502550L,99801780128L,99800076039L,99800080915L,99801015197L,99801233125L,99800675649L,99800888917L,99800813388L,99801447623L,99801287056L,99800210881L,99801108785L,99801476870L,99801506874L,99801481934L,99800423983L,99800434181L,99801089998L,99801606223L,99801704117L,99801747926L,99801291893L,99800567913L,99800892834L,99801069504L,99800034094L,99800168653L,99800301023L)

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

  var lambda_S: Double = 0.3

  //  Possibility of Lognormal Residence Times
  //  ******************************************
  //  All the samples drawn from these distributions represent days
  final val exposedDurationProbabilityDistribution = LogNormal(4.5, 1.5)
  final val presymptomaticDurationProbabilityDistribution = LogNormal(1.1, 0.9)
  final val asymptomaticDurationProbabilityDistribution = LogNormal(8, 2)
  final val mildSymptomaticDurationProbabilityDistribution = LogNormal(8, 2)
  final val mildToSevereSymptomaticDurationProbabilityDistribution = LogNormal(6.6, 4.9)
  final val severeSymptomaticDurationProbabilityDistribution = LogNormal(1.5, 2.0)
  final val hospitalisedDurationProbabilityDistribution = LogNormal(18.1, 6.3)

//  //  Possibility of Exponential Residence Times
//  //  ******************************************
//  //  All the samples drawn from these distributions represent days
  //  final val exposedDurationProbabilityDistribution = Exponential(4.5)
  //  final val presymptomaticDurationProbabilityDistribution = Exponential(1.1)
  //  final val asymptomaticDurationProbabilityDistribution = Exponential(4.5)
  //  final val mildSymptomaticDurationProbabilityDistribution = Exponential(8)
  //  final val severeSymptomaticDurationProbabilityDistribution = Exponential(1.5)
  //  final val HospitalisedProbabilityDuration = Exponential(18.1)

  //**********************************************************************************//


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
  var initialRecoveredFraction: Float     = 00f / 100 //TODO: Check default values. Used to be 30,20,10
  var prevaccinatedOneShotFraction: Float = 00f / 100
  var prevaccinatedTwoShotFraction: Float = 00f / 100

  var vaccinationRate: Double = 0.5d / 100d

  // *************************************************//

  // FOI Details *************************************//
  var alpha = 0.0

  def backgroundFOI(t: Double, tpeak: Double = 8.5, width: Double = 10): Double = {
    alpha * 0.07 * math.exp(-math.pow((t - tpeak), 2) / (2 * width))
  }

  // **************************************************//

}
