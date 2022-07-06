package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given

import scala.collection.parallel.CollectionConverters.given
import scala.math.{pow, acos, sin, cos, abs, Pi, sqrt}

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.types.*

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  import org.apache.spark.sql.SparkSession

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Visualization")
      .master("local[*]")
      .getOrCreate()

  import spark.implicits._
  import scala3encoders.given


  case class TempColor(temp: Temperature, color: Color)
  case class Neighbors(min: TempColor, max: TempColor)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)],
      location: Location): Temperature = {

    def calcDist(loc: Location): Double =
      val r = 6371 // Radius of the Earth

      val isAntipodes: Boolean =
        if (location.lat != -loc.lat) false
        else if (location.lon > 0) (location.lon - loc.lon) == 180
        else (location.lon - loc.lon) == -180

      if (location == loc) 0
      else if (isAntipodes) Pi
      else acos(sin(location.lat)*sin(loc.lat)
            + cos(location.lat)*cos(loc.lat)*cos(abs(location.lon + loc.lon)))

    def weight(loc: Location): Double =
      val p = 2
      val dist = calcDist(loc)

      if(dist < 1) 1
      else 1 / pow(dist, p)

    def seqOp(e: (Location, Temperature)): (Double, Double) =
      val w = weight(e._1)
      (w * e._2, w)

    def combOp(a: (Double, Double), b: (Double, Double)): (Double, Double) =
      (a._1 + b._1, a._2 + b._2)

    val acc: (Double, Double) = temperatures.map(seqOp).reduce(combOp)

    acc._1 / acc._2
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    val zeroValue = Neighbors(TempColor(Double.MinValue, Color(0, 0, 0)),
                              TempColor(Double.MaxValue, Color(0, 0, 0)))

    def seqOp(acc: Neighbors, e: TempColor): Neighbors =
      if (acc.min.temp < e.temp && e.temp <= value) Neighbors(e, acc.max)
      else if (value <= e.temp && e.temp < acc.max.temp) Neighbors(acc.min, e)
      else acc

    val neighbors = points.map((temp, color) => TempColor(temp, color))
                          .foldLeft(zeroValue)(seqOp)

    def interpolate(ns: Neighbors): Color =
      // For each RGB
      def calc(intOf: Color => Int): Int =
        val num = intOf(ns.min.color) * (ns.max.temp - value)
                  + intOf(ns.max.color) * (value - ns.min.temp)
        val den = ns.max.temp - ns.min.temp
        (num / den).toInt

      Color(calc(c => c._1), calc(c => c._2), calc(c => c._3))

    // Out of bound value
    if (neighbors.min.temp == Double.MinValue) neighbors.max.color
    else if (neighbors.max.temp == Double.MaxValue) neighbors.min.color
    else interpolate(neighbors)


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)],
                colors: Iterable[(Temperature, Color)]): ImmutableImage =
    val width = 360
    val height = 180

    def calcPixel(xy: (Int, Int)): Pixel =
      val temp = predictTemperature(temperatures,
                      Location(-xy._1 + (height/2), xy._2 - (width/2)))
      val color = interpolateColor(colors, temp)
      Pixel(xy._1, xy._2, color.red, color.green, color.blue, 255)

    val cords = for {y <- 0 until width; x <- 0 until height} yield (x, y)
    val pixels = cords.par.map(calcPixel)

    ImmutableImage.wrapPixels(width, height, pixels.toArray, ImageMetadata.empty)
