package observatory

import java.time.LocalDate
import scala.io.Source
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.types.*

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

  import spark.implicits._
  import scala3encoders.given

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String,
      temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] =
    
    def getDataFrame(resourcePath: String, schema: StructType): DataFrame =
      val lines = Source.fromInputStream(getClass.getResourceAsStream(resourcePath), "utf-8")
                    .getLines()
                    .toList

      val ds = spark.sparkContext.parallelize(lines).toDS

      spark.read.options(Map("header" -> "false"))
        .schema(schema)
        .csv(ds)

    val stnSchema = new StructType()
                      .add("stn", StringType, true)
                      .add("wban", StringType, true)
                      .add("lat", DoubleType, true)
                      .add("lon", DoubleType, true)

    val tempSchema = new StructType()
                      .add("stn", StringType, true)
                      .add("wban", StringType, true)
                      .add("mon", IntegerType, true)
                      .add("day", IntegerType, true)
                      .add("temp", DoubleType, true)

    val dfs = getDataFrame(stationsFile, stnSchema)
              .na.drop(Seq("lat", "lon")) // No locations are ignored
    val dft = getDataFrame(temperaturesFile, tempSchema)

    // Join
    val df = dfs.join(dft, List("stn", "wban"))

    // Convert Farenheit to Celsious; (x°F − 32) × 5/9
    def toCelcious(farenheit: Double): Double = (farenheit - 32) * 5 / 9

    val rdd = df.map(r => (r.getAs[Int]("mon"), r.getAs[Int]("day"),
                           Location(r.getAs[Double]("lat"), r.getAs[Double]("lon")),
                           toCelcious(r.getAs[Double]("temp")))).rdd

    rdd.map((mon, day, loc, temp) => (LocalDate.of(year, mon, day), loc, temp))
       .collect().toSeq


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)])
      : Iterable[(Location, Temperature)] =
    val lists = records.map((dt, loc, temp) => (loc, temp)).toList
    val rdd = spark.sparkContext.parallelize(lists)

    rdd.aggregateByKey((0.0, 0))((acc, v) => (acc._1 + v, acc._2 + 1),
                                  (a, b) => (a._1 + b._1, a._2 + b._2))
       .mapValues((sum, cnt) => sum / cnt).collect().toSeq
