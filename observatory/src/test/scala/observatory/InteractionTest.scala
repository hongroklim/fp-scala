package observatory

import scala.collection.concurrent.TrieMap
import java.awt.Point
import org.scalacheck.Prop
import org.scalacheck.Prop.{forAll, propBoolean}
import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.Colors
import scala.math.BigDecimal.RoundingMode

trait InteractionTest extends MilestoneSuite:
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  test("location"){
    val tile = Tile(0, 0, 0)
    assertEquals(math.round(Interaction.tileLocation(tile)._1), math.round(85.0511))
  }
