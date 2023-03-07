package observatory

import java.time.LocalDate
import java.nio.file.{Paths, Path}

import org.apache.spark.rdd.RDD
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface:
  import org.apache.spark.sql.SparkSession

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Extraction")
      .master("local")
      .getOrCreate()


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] =
    def readTextFile(path: String): RDD[String] = {
      val lines = Source.fromInputStream(getClass.getResourceAsStream(path), "utf-8")
        .getLines()
        .toSeq
  
      spark.sparkContext.parallelize(lines)
    }

    val stations = readTextFile(stationsFile)
      .map(_.split(','))
      .filter(_.length == 4)
      .map(a => ((a(0), a(1)), Location(a(2).toDouble, a(3).toDouble)))

    def toCelsius(fahrenheit: Temperature): Temperature =
      (fahrenheit - 32) / 1.8

    val temperatures = readTextFile(temperaturesFile)
      .map(_.split(','))
      .filter(_.length == 5)
      .map(a => ((a(0), a(1)), (LocalDate.of(year, a(2).toInt, a(3).toInt), toCelsius(a(4).toDouble))))
    
    stations.join(temperatures)
      .mapValues(v => (v._2._1, v._1, v._2._2))
      .values
      .collect()
      .toSeq


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    val rdd: RDD[(LocalDate, Location, Temperature)] = spark.sparkContext.parallelize(records.toSeq)
    
    rdd
      .groupBy(_._2)  // Location
      .mapValues(e => e.map(e => (e._3, 1)))  // Temperature, 1
      .mapValues(_.reduce((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2)))  // Add
      .mapValues((t, n) => t / n)
      .collect()
      .toSeq
