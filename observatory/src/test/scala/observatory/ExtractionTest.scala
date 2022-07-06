package observatory

import org.apache.log4j.{Logger, Level}

trait ExtractionTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _
  
  // Implement tests for the methods of the `Extraction` object
  import Extraction.*

  /*
  test("locateTemperatures()") {
    println(locateTemperatures(1975, "/stations.csv", "/1975.csv").take(3))
  }

  test("yearAvg") {
    val records = locateTemperatures(1975, "/stations.csv", "/1975.csv").take(10)
    println(locationYearlyAverageRecords(records))
  }
  */
