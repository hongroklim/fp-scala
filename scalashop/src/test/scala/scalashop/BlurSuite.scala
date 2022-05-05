package scalashop

import java.util.concurrent._
import scala.collection._

class BlurSuite extends munit.FunSuite {
  test("RGBA") {
    val point = rgba(10, 20, 35, 40)
    assertEquals(red(point), 10)
    assertEquals(green(point), 20)
    assertEquals(blue(point), 35)
    assertEquals(alpha(point), 40)
  }

  test("boxBlurKernel") {
    val img = new Img(2, 1, Array(rgba(10, 20, 30, 40), rgba(20, 10, 40, 30))) 
    assert(boxBlurKernel(img, 0, 0, 1) > 0)
  }

  test("boxBlurKernel : edge pixels") {
    val img = new Img(3, 4, (0 to 11).toArray)
    assertEquals(boxBlurKernel(img, 0, 2, 1), 13)
  }
}
