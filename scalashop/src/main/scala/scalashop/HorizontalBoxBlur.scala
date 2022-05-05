package scalashop

import java.util.concurrent._
import org.scalameter._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 5,
    Key.exec.maxWarmupRuns := 10,
    Key.exec.benchRuns := 10,
    Key.verbose := false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // implement this method using the `boxBlurKernel` method
    def iter(x: Int, y: Int): Unit =
      if (y >= end) ()
      else if (x >= src.width) iter(0, y+1)
      else { dst.update(x, y, boxBlurKernel(src, x, y, radius)); iter(x+1, y) }

    iter(0, from)
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // implement using the `task` construct and the `blur` method
    val splits: List[(Int, Int)] = {
      val step = scala.math.ceil(src.height / numTasks.toFloat).toInt
      val points = ((0 to src.height by step).toList
            ++ (if (src.height % numTasks != 0) List(src.height) else List()))

      points.zip(points.tail)
    }

    def genTasks(splits: List[(Int, Int)], acc: List[ForkJoinTask[Unit]]): List[ForkJoinTask[Unit]] =
      if (splits.isEmpty) acc
      else genTasks(splits.tail,
        task(blur(src, dst, splits.head._1, splits.head._2, radius)) :: acc)

    val tasks = genTasks(splits, List())

    tasks foreach (t => t.join())
  }
}
