package com.thesamet.spatial

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._

/** DimensionalOrdering is a trait whose instances each represent a strategy for ordering instances
  * of a multidimensional type by a projection on a given dimension.
  */
trait DimensionalOrdering[A] {
  /** How many dimensions type A has. */
  def dimensions: Int

  /** Returns an integer whose sign communicates how x's projection on a given dimension compares
    * to y's.
    *
    * Denote the projection of x and y on `dimension` by x' and y' respectively. The result sign has
    * the following meaning:
    *
    * - negative if x' < y'
    * - positive if x' > y'
    * - zero if x' == y'
    */
  def compareProjection(dimension: Int)(x: A, y: A): Int

  /** Returns an Ordering of A in which the given dimension is the primary ordering criteria.
    * If x and y have the same projection on that dimension, then they are compared on the lowest
    * dimension that is different.
    */
  def orderingBy(dimension: Int): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A): Int = {
      @tailrec
      def compare0(cd: Int): Int = {
        if (cd == dimensions) 0
        else {
          val c = compareProjection(cd)(x, y)
          if (c != 0) c
          else compare0(cd + 1)
        }
      }

      compareProjection(dimension)(x, y) match {
        case t if t != 0 => t
        case 0 => compare0(0)
      }
    }
  }
}

object DimensionalOrdering {
  implicit def dimOrderingFromTuple2[A](implicit ord: Ordering[A]) =
      new DimensionalOrdering[(A, A)] {
        def dimensions = 2
        def compareProjection(d: Int)(x: (A, A), y: (A, A)) = ord.compare(
          x.productElement(d).asInstanceOf[A], y.productElement(d).asInstanceOf[A])
      }
}

/** Metric is a trait whose instances each represent a way to measure distances between
  * instances of a type.
  *
  * `A` represents the type of the points and `R` represents the metric value.
  */
trait Metric[A, R] {
  /** Returns the distance between two points. */
  def distance(x: A, y: A): R

  /** Returns the distance between x and a hyperplane that passes through y and perpendicular to
    * that dimension.
    */
  def planarDistance(dimension: Int)(x: A, y: A): R
}

object Metric {
  implicit def metricFromTuple2[A](implicit n: Numeric[A]) = new Metric[(A, A), A] {
    def distance(x: (A, A), y: (A, A)): A = {
      val dx = (x._1 - y._1)
      val dy = (x._2 - y._2)
      dx * dx + dy * dy
    }
    def planarDistance(d: Int)(x: (A, A), y: (A, A)): A = {
      val dd = x.productElement(d).asInstanceOf[A] - y.productElement(d).asInstanceOf[A]
      dd * dd
    }
  }
}

class KDTree[A] private (root: KDTreeNode[A]) {
  def size: Int = root.size
  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], numeric: Numeric[R]): Seq[A] =
      root.findNearest(x, n)
}

class KDTreeMap[A, B] private (root: KDTreeNode[(A, B)]) {
  def size: Int = root.size
  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], numeric: Numeric[R]): Seq[(A, B)] =
      root.findNearest((x, null.asInstanceOf[B]), n)(KDTreeMap.liftMetric(metric), numeric)
}

sealed trait KDTreeNode[A] {
  override def toString = toStringSeq(0) mkString "\n"
  def toStringSeq(indent: Int): Seq[String]
  def size: Int
  def isEmpty: Boolean
  def findNearest0[R](x: A, n: Int, skipParent: KDTreeNode[A], values: Seq[(A, R)])(
      implicit metric: Metric[A, R], ord: Ordering[R]): Seq[(A, R)]
  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], ord: Ordering[R]): Seq[A]
  def findMinimalParent(x: A, withSize: Int): KDTreeNode[A]
  def valueSeq: Seq[A]
}

case class KDTreeInnerNode[A](dim: Int, point: A, below: KDTreeNode[A],
                              above: KDTreeNode[A])(ordering: Ordering[A])
    extends KDTreeNode[A] {
  def toStringSeq(indent:Int) = {
    val i = "  " * indent

    Seq(i + "size=%d dim=%d point=%s ".format(size, dim,

     point),
        i + "Below:") ++ below.toStringSeq(indent + 1) ++ Seq(i + "Above:") ++
        above.toStringSeq(indent + 1)
  }

  val size = below.size + above.size + 1

  def isEmpty = false

  def isBelow(x: A) = ordering.lt(x, point)

  def isAbove(x: A) = ordering.gt(x, point)

  def findMinimalParent(x: A, withSize: Int): KDTreeNode[A]  = {
    if (isBelow(x)) {
      if (below.size < withSize) this
      else below.findMinimalParent(x, withSize)
    } else if (isAbove(x)) {
      if (above.size < withSize) this
      else above.findMinimalParent(x, withSize)
    } else this
  }

  def valueSeq: Seq[A] = below.valueSeq ++ above.valueSeq :+ point

  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], ord: Ordering[R]): Seq[A] = {
    // Build initial set of candidates from the smallest subtree containing x with at least n
    // points.
    val p = findMinimalParent(x, withSize=n)
    val values = p.valueSeq map { p => (p, metric.distance(x, p)) } sortBy(_._2) take(n)
    findNearest0(x, n, p, values) map { _._1 }
  }

  def findNearest0[R](x: A, n: Int, skipParent: KDTreeNode[A], values: Seq[(A, R)])(
      implicit metric: Metric[A, R], ord: Ordering[R]): Seq[(A, R)] = {
    if (skipParent eq this) values
    else {
      val myDist = metric.distance(point, x)
      val currentBest = values.last._2

      val newValues = if (myDist < currentBest) {
        (values :+ (point, myDist)) sortBy(_._2) take(n)
      }
      else values
      val newCurrentBest = values.last._2

      val dp = metric.planarDistance(dim)(x, point)

      if (dp < newCurrentBest) {
        val values2 = above.findNearest0(x, n, skipParent, newValues)
        below.findNearest0(x, n, skipParent, values2)
      } else if (isAbove(x)) {
        above.findNearest0(x, n, skipParent, newValues)
      } else if (isBelow(x)) {
        below.findNearest0(x, n, skipParent, newValues)
      } else sys.error("Unexpected value!")
    }
  }
}

case class KDTreeEmpty[A]() extends KDTreeNode[A] {
  def toStringSeq(indent: Int) = Seq(("  " * indent) + "[Empty]")
  def size = 0
  def isEmpty = true
  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], ord: Ordering[R]): Seq[A] =
      Seq.empty
  def findNearest0[R](x: A, n: Int, skipParent: KDTreeNode[A], values: Seq[(A, R)])(
      implicit metric: Metric[A, R], ord: Ordering[R]): Seq[(A, R)] = values
  def findMinimalParent(x: A, withSize: Int): KDTreeNode[A] = sys.error("Unexpected.")
  def valueSeq: Seq[A] = Seq.empty
}

object KDTreeNode {
  def buildTreeNode[A](depth: Int, points: Seq[A])(
      implicit ord: DimensionalOrdering[A]): KDTreeNode[A] = {
    def findSplit(points: Seq[A], i: Int): (A, Seq[A], Seq[A]) = {
      val sp = points.sorted(ord.orderingBy(i))
      val medIndex = sp.length / 2
      (sp(medIndex), sp.take(medIndex), sp.drop(medIndex + 1))
    }

    if (points.isEmpty) KDTreeEmpty[A]
    else {
      val i = depth % ord.dimensions
      val (median, below, above) = findSplit(points, i)
      KDTreeInnerNode(
        i, median, buildTreeNode(depth+1, below),
        buildTreeNode(depth + 1, above))(ord.orderingBy(i))
    }
  }
}

object KDTree {
  def buildTree[A](points: A*)(implicit ord: DimensionalOrdering[A]) = {
    assert(ord.dimensions >= 1)
    new KDTree(KDTreeNode.buildTreeNode(0, points))
  }
}

object KDTreeMap {
  def buildTreeMap[A, B](points: (A, B)*)(implicit ord: DimensionalOrdering[A]) =
    new KDTreeMap(KDTreeNode.buildTreeNode(0, points))
  implicit def liftMetric[A, B, R](implicit metric: Metric[A, R]): Metric[(A, B), R] =
      new Metric[(A, B), R] {
    def distance(x: (A, B), y: (A, B)) = metric.distance(x._1, y._1)
    def planarDistance(dimension: Int)(x: (A, B), y: (A, B)) =
      metric.planarDistance(dimension)(x._1, y._1)
  }

  implicit def liftDimensionalOrdering[A, B](implicit ord: DimensionalOrdering[A]):
      DimensionalOrdering[(A, B)] = new DimensionalOrdering[(A, B)] {
        val dimensions = ord.dimensions
        def compareProjection(dim: Int)(x: (A, B), y: (A, B)) = ord.compareProjection(dim)(x._1, y._1)
      }
}
