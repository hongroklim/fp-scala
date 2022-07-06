package observatory

import scala.math.{abs, sqrt}

trait VisualizationTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object
  import Visualization.*
  import com.sksamuel.scrimage.implicits._

  val colors: Iterable[(Temperature, Color)] =
    Seq(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0))
    )

  test("Predict Temperature") {
    val temperatures: Iterable[(Location, Temperature)] =
      Seq(
        (Location(1.0, 1.0), 1.0),
        (Location(-1.0, 1.0), 1.0),
        (Location(1.0, -1.0), 1.0),
        (Location(-1.0, -1.0), 1.0)
      )
    assertEquals(predictTemperature(temperatures, Location(0.0, 0.0)), 1.0)
  }

  test("Interpolate Color") {
    assertEquals(interpolateColor(colors, 0), Color(0, 255, 255))
    assertEquals(interpolateColor(colors, 20).red, 255)
    assertEquals(interpolateColor(colors, -23).blue, 255)
  }

  test("Interpolate Color Between") {
    val colors: Iterable[(Temperature, Color)] =
      Seq(
        (100, Color(255, 0, 0)),
        (-100, Color(0, 0, 255))
      )
    
    val color = interpolateColor(colors, 10)
    assertEquals(255 - color.red < 255 - color.blue, true)
  }

  test("Visualize") {
    val temperatures: Iterable[(Location, Temperature)] =
      Seq(
        (Location(90.0, -180.0), 60.0),
        (Location(90.0, 180.0), 10.0),
        (Location(-90.0, 180.0), -10.0),
        (Location(-90.0, -180.0), -60.0)
      )
    val image = visualize(temperatures, colors)
    image.output(new java.io.File("target/some-image.png"))

    /*
    assertEquals(image.pixel(10, 10).red(), 255)
    assertEquals(image.pixel(10, 10).green(), 0)
    assertEquals(image.pixel(10, 10).blue(), 255)
    */
  }
  
