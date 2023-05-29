package schools

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.listeners.CSVSpecs
import com.bharatsim.engine.models.Node
import schools.InfectionStatus._
import schools.Main.n_grid
import schools.models.{Home, Person}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GriddedOutputSpec(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] = List("#Day", "Latitude", "Longitude", "InfectedCount", "RecoveredCount")

  private def roundLatLong(lat: String, long: String): (Double, Double) = {
    val scale = 3  // number of digits after decimal point
    (
      BigDecimal(lat).setScale(scale, BigDecimal.RoundingMode.DOWN).toDouble,
      BigDecimal(long).setScale(scale, BigDecimal.RoundingMode.DOWN).toDouble
    )
  }
  override def getRows(): List[List[Any]] = {
    if (context.getCurrentStep % (15 * Disease.inverse_dt) == 0) {  // get data only every 5 days
      val label = "Person"
      val infectedStates = List(Exposed.toString, Asymptomatic.toString, Presymptomatic.toString, InfectedMild.toString, InfectedSevere.toString, Hospitalised.toString)

      val gridCounts= Array.ofDim[Int](n_grid, n_grid)

      val people = context.graphProvider.fetchNodes(label)

      people.foreach((p) => {
        val infectionState = p.getParams.apply("infectionState").toString
        if (infectedStates.contains(infectionState)) {

          val agent = p.as[Person]
          val home = agent.getConnections(agent.getRelation("Home").get).toList.head.as[Home]

          val grid = home.gridBox

          gridCounts(grid._2)(grid._1) = gridCounts(grid._2)(grid._1) + 1;
        }
      })

      val rows = ListBuffer.empty[List[String]]

      for (i <-0 to n_grid-1) {
        var allcols = ListBuffer[String]()
        for (j <- 0 to n_grid-1) {
          allcols += gridCounts(i)(j).toString +" "
        }
        rows.addOne(allcols.toList)
      }

      return rows.toList
    }
    else {
      List.empty
    }
  }
}
