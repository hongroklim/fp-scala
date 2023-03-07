package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location =
    val lat: Double = math.atan(math.sinh(math.Pi - tile.y / math.pow(2, tile.zoom) * 2 * math.Pi)) * 180 / math.Pi
    val lon: Double = tile.x / math.pow(2, tile.zoom) * 360 - 180

    Location(lat, lon)


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage =
    val locations: Seq[Location] =
      for {
        i <- 0 until 256
        j <- 0 until 256
      } yield tileLocation(Tile(tile.x+i, tile.y+j, tile.zoom))

    val pixels: Array[Pixel] = locations
      .map(Visualization.predictTemperature(temperatures, _))
      .map(Visualization.interpolateColor(colors, _))
      .map(color => Pixel(color.red, color.green, color.blue))
      .toArray

    ImmutableImage.create(256, 256, pixels)


  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit =
    val tiles = for {
      zoom <- 0 until 4
      x <- 0 until math.pow(2, zoom).toInt
      y <- 0 until math.pow(2, zoom).toInt
      yearData <- yearlyData
    } yield generateImage(yearData._1, Tile(x, y, zoom), yearData._2)

