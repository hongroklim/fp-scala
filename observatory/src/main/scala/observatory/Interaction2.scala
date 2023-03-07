package observatory

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 extends Interaction2Interface:

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] =
    List(
        Layer(LayerName.Temperatures,
          //customize color setting
          Seq(
            (60.0, Color(255, 255, 255)),
            (32.0, Color(255, 0, 0)),
            (12.0, Color(255, 255, 0)),
            (0.0, Color(0, 255, 255)),
            (-15.0, Color(0, 0, 255)),
            (-27.0, Color(255, 0, 255)),
            (-50.0, Color(33, 0, 107)),
            (-60.0, Color(0, 0, 0))),
          //available dataset
          1975 to 2015
        ),
        Layer(LayerName.Deviations,
          //customize color setting
          Seq(
            (7.0, Color(0, 0, 0)),
            (4.0, Color(255, 0, 0)),
            (2.0, Color(255, 255, 0)),
            (0.0, Color(255, 255, 255)),
            (-2.0, Color(0, 255, 255)),
            (-7.0, Color(0, 0, 255))),
          1975 to 2015
        ))

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] =
    Signal(selectedLayer.apply().bounds)

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] =
    Signal(sliderValue.apply().max(yearBounds(selectedLayer).apply().min).min(yearBounds(selectedLayer).apply().max))


  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] =
    Signal(s"generated/${selectedLayer.apply().layerName.id}/${selectedYear.apply()}/{z}/{x}/{y}.png")

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] =
    Signal(s"${selectedLayer.apply().layerName.id.capitalize} (${selectedYear.apply().toString()})")


// Interface used by the grading infrastructure. Do not change signatures
// or your submission will fail with a NoSuchMethodError.
trait Interaction2Interface:
  def availableLayers: Seq[Layer]
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range]
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year]
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String]

enum LayerName:
  case Temperatures, Deviations
  def id: String =
    this.match
      case Temperatures => "temperatures"
      case Deviations => "deviations"

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

