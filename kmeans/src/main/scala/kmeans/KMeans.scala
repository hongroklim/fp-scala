package kmeans

import scala.annotation.tailrec
import scala.collection.{Map, Seq, mutable}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ForkJoinTaskSupport, ParMap, ParSeq}
import scala.util.Random
import org.scalameter._
import java.util.concurrent.ForkJoinPool

class KMeans extends KMeansInterface {

  def generatePoints(k: Int, num: Int): ParSeq[Point] = {
    val randx = new Random(1)
    val randy = new Random(3)
    val randz = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randz.nextDouble() * 0.5
        new Point(x, y, z)
      }).to(mutable.ArrayBuffer).par
  }

  def initializeMeans(k: Int, points: ParSeq[Point]): ParSeq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to(mutable.ArrayBuffer).par
  }

  def findClosest(p: Point, means: IterableOnce[Point]): Point = {
    val it = means.iterator
    assert(it.nonEmpty)
    var closest = it.next()
    var minDistance = p.squareDistance(closest)
    while (it.hasNext) {
      val point = it.next()
      val distance = p.squareDistance(point)
      if (distance < minDistance) {
        minDistance = distance
        closest = point
      }
    }
    closest
  }

  def classify(points: ParSeq[Point], means: ParSeq[Point]): ParMap[Point, ParSeq[Point]] = {
    points.groupBy(p => findClosest(p, means)).par
  }

  def findAverage(oldMean: Point, points: ParSeq[Point]): Point = if (points.isEmpty) oldMean else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: ParMap[Point, ParSeq[Point]], oldMeans: ParSeq[Point]): ParSeq[Point] = {
    oldMeans.map(m => (m, classified.get(m)))
            .map{ case (k, Some(v)) => findAverage(k, v)
                  case (k, None) => new Point(0, 0, 0) }
  }

  def converged(eta: Double, oldMeans: ParSeq[Point], newMeans: ParSeq[Point]): Boolean = {
    (0 until oldMeans.length)
      .forall(i => oldMeans(i).squareDistance(newMeans(i)) <= eta)
  }

  @tailrec
  final def kMeans(points: ParSeq[Point], means: ParSeq[Point], eta: Double): ParSeq[Point] = {
    val newMeans = update(classify(points, means), means)
    if (converged(eta, means, newMeans)) kMeans(points, newMeans, eta) else means
  }
}

/** Describes one point in three-dimensional space.
 *
 *  Note: deliberately uses reference equality.
 */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = {
    square(that.x - x)  + square(that.y - y) + square(that.z - z)
  }
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns := 20,
    Key.exec.maxWarmupRuns := 40,
    Key.exec.benchRuns := 25,
    Key.verbose := false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqtime = {
      // Retrieve the support created to run the algorithm in parallel
      val parTasksupport = points.tasksupport
      // Create a support with only one executor to run the algorithm sequentially
      val seqPool = new ForkJoinPool(1)
      val seqTasksupport = new ForkJoinTaskSupport(seqPool)
      try {
        // Run the the algorithm on the sequential support
        points.tasksupport = seqTasksupport
        means.tasksupport = seqTasksupport
        // Measure performances on the sequential runner
        standardConfig measure {
          kMeans.kMeans(points, means, eta)
        }
      }
      finally {
        // Restore the parallel support
        points.tasksupport = parTasksupport
        means.tasksupport = parTasksupport
        // Stop the sequential runner
        seqPool.shutdown()
      }
    }

    // Measure performances on the parallel runner
    val partime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }

    println(s"sequential time: $seqtime")
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}
