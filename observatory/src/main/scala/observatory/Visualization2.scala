package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface:

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature =
    d00*(1-point.x)*(1-point.y) + d10*point.x*(1-point.y) +
    d01*(1-point.x)*point.y + d11*point.x*point.y


  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): ImmutableImage =
    val locations =
      for {
        i <- 0 until 256
        j <- 0 until 256
      } yield Interaction.tileLocation(Tile(tile.x+i, tile.y+j, tile.zoom))

    val pixels = locations
      .map(location => {
        val x1 = location.lon - location.lon.floor.toInt
        val y1 = location.lat.ceil.toInt - location.lat
        val d00 = grid(GridLocation(location.lat.ceil.toInt, location.lon.floor.toInt))
        val d01 = grid(GridLocation(location.lat.floor.toInt, location.lon.floor.toInt))
        val d10 = grid(GridLocation(location.lat.ceil.toInt, location.lon.ceil.toInt))
        val d11 = grid(GridLocation(location.lat.floor.toInt, location.lon.ceil.toInt))
        bilinearInterpolation(CellPoint(x1, y1), d00, d01, d10, d11)
      })
      .map(temperature => Visualization.interpolateColor(colors, temperature))
      .map(color => Pixel(color.red, color.green, color.blue))
      .toArray

    ImmutableImage.create(256, 256, pixels)
