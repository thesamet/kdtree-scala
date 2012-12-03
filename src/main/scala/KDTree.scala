package com.thesamet.spatial

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import scala.collection.{IterableLike, MapLike}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ArrayBuffer, Builder}

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
  def dimensionalOrderingForTuple[T <: Product,A](dim: Int)(implicit ord: Ordering[A]) =
    new DimensionalOrdering[T] {
      val dimensions = dim
      def compareProjection(d: Int)(x: T, y: T) = ord.compare(
        x.productElement(d).asInstanceOf[A], y.productElement(d).asInstanceOf[A])
    }

  implicit def dimensionalOrderingForTuple2[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A), A](2)

  implicit def dimensionalOrderingForTuple3[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A, A), A](3)

  implicit def dimensionalOrderingForTuple4[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A, A, A), A](4)

  implicit def dimensionalOrderingForTuple5[A](implicit ord: Ordering[A]) =
    dimensionalOrderingForTuple[(A, A, A, A, A), A](5)
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

class KDTree[A] private (root: KDTreeNode[A, Boolean])(implicit ord: DimensionalOrdering[A]) extends IterableLike[A, KDTree[A]] {
  override def seq = this

  override def size: Int = root.size

  override def iterator: Iterator[A] = root.toStream.iterator map (_. _1)

  def contains(x: A): Boolean = root.get(x).isDefined

  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], numeric: Numeric[R]): Seq[A] =
      root.findNearest(x, n) map (_._1)

  def newBuilder: Builder[A, KDTree[A]] = KDTree.newBuilder
}

class KDTreeMap[A, B] private (root: KDTreeNode[A, B])(implicit ord: DimensionalOrdering[A])
    extends Map[A, B] with MapLike[A, B, KDTreeMap[A, B]] {

  override def empty: KDTreeMap[A, B] = KDTreeMap.empty[A, B](ord)

  override def size: Int = root.size

  override def iterator: Iterator[(A, B)] = root.toStream.iterator

  def get(x: A): Option[B] = root.get(x)

  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], numeric: Numeric[R]): Seq[(A, B)] =
      root.findNearest(x, n)

  def +[B1 >: B](kv: (A, B1)): KDTreeMap[A, B1] = KDTreeMap.fromSeq(toSeq ++ Seq(kv))
  def -(key: A): KDTreeMap[A, B] = KDTreeMap.fromSeq(toSeq.filter(_._1 != key))
}

sealed trait KDTreeNode[A, B] {
  override def toString = toStringSeq(0) mkString "\n"
  def toStringSeq(indent: Int): Seq[String]
  def size: Int
  def isEmpty: Boolean
  def findNearest0[R](x: A, n: Int, skipParent: KDTreeNode[A, B], values: Seq[((A, B), R)])(
      implicit metric: Metric[A, R], ord: Ordering[R]): Seq[((A, B), R)]
  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], ord: Ordering[R]): Seq[(A, B)]
  def toStream: Stream[(A, B)]
  def toSeq: Seq[(A, B)]

  @tailrec
  final def get(x: A): Option[B] = this match {
    case n@ KDTreeInnerNode(dim, k, v, below, above) =>
    if (k == x) Some(v)
    else (if (n.isAbove(x)) above else below).get(x)
    case n@ KDTreeEmpty() => None
  }
}

case class KDTreeInnerNode[A, B](
    dim: Int, key: A, value: B, below: KDTreeNode[A, B], above: KDTreeNode[A, B])(
    ordering: Ordering[A]) extends KDTreeNode[A, B] {
  def toStringSeq(indent:Int) = {
    val i = "  " * indent

    Seq(i + "size=%d dim=%d key=%s ".format(size, dim, key),
        i + "Below:") ++ below.toStringSeq(indent + 1) ++ Seq(i + "Above:") ++
        above.toStringSeq(indent + 1)
  }

  val size = below.size + above.size + 1

  def isEmpty = false

  def isBelow(x: A) = ordering.lt(x, key)

  def isAbove(x: A) = ordering.gt(x, key)

  def isEquiv(x: A) = ordering.equiv(x, key)

  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], ord: Ordering[R]): Seq[(A, B)] = {
    // Build initial set of candidates from the smallest subtree containing x with at least n
    // points.
    val minParent = KDTreeNode.findMinimalParent(this, x, withSize=n)
    val values = (minParent.toSeq.map {p => (p, metric.distance(x, p._1)) }).sortBy(_._2).take(n)
    findNearest0(x, n, minParent, values) map { _._1 }
  }

  def findNearest0[R](x: A, n: Int, skipParent: KDTreeNode[A, B], values: Seq[((A, B), R)])(
      implicit metric: Metric[A, R], ord: Ordering[R]): Seq[((A, B), R)] = {
    if (skipParent eq this) values
    else {
      val myDist = metric.distance(key, x)
      val currentBest = values.last._2

      val newValues = if (myDist < currentBest) {
        (values :+ ((key, value), myDist)) sortBy(_._2) take(n)
      }
      else values
      val newCurrentBest = values.last._2

      val dp = metric.planarDistance(dim)(x, key)

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

  def toStream: Stream[(A, B)] = below.toStream ++ Stream((key, value)) ++ above.toStream

  def toSeq: Seq[(A, B)] = below.toSeq ++ Seq((key, value)) ++ above.toSeq
}

case class KDTreeEmpty[A, B]() extends KDTreeNode[A, B] {
  def toStringSeq(indent: Int) = Seq(("  " * indent) + "[Empty]")
  def size = 0
  def isEmpty = true
  def findNearest[R](x: A, n: Int)(implicit metric: Metric[A, R], ord: Ordering[R]): Seq[(A, B)] =
      Seq.empty
  def findNearest0[R](x: A, n: Int, skipParent: KDTreeNode[A, B], values: Seq[((A, B), R)])(
      implicit metric: Metric[A, R], ord: Ordering[R]): Seq[((A, B), R)] = values
  def toSeq: Seq[(A, B)] = Seq.empty
  def toStream: Stream[(A, B)] = Stream.empty
}

object KDTreeNode {
  def buildTreeNode[A, B](depth: Int, points: Seq[(A, B)])(
      implicit ord: DimensionalOrdering[A]): KDTreeNode[A, B] = {
    def findSplit(points: Seq[(A, B)], i: Int): ((A, B), Seq[(A, B)], Seq[(A, B)]) = {
      val sp = points.sortBy(_._1)(ord.orderingBy(i))
      val medIndex = sp.length / 2
      (sp(medIndex), sp.take(medIndex), sp.drop(medIndex + 1))
    }

    if (points.isEmpty) KDTreeEmpty[A, B]
    else {
      val i = depth % ord.dimensions
      val ((key, value), below, above) = findSplit(points, i)
      KDTreeInnerNode(
        i, key, value, buildTreeNode(depth+1, below),
        buildTreeNode(depth + 1, above))(ord.orderingBy(i))
    }
  }

  @tailrec
  def findMinimalParent[A, B](node: KDTreeInnerNode[A, B], x: A, withSize: Int):
      KDTreeInnerNode[A, B] = if (node.key == x) node
      else {
        val next = if (node.isBelow(x)) node.below else node.above
        if (next.size < withSize) node
        else findMinimalParent(next.asInstanceOf[KDTreeInnerNode[A, B]], x, withSize)
      }
}

object KDTree {
  def apply[A](points: A*)(implicit ord: DimensionalOrdering[A]) = fromSeq(points)

  def fromSeq[A](points: Seq[A])(implicit ord: DimensionalOrdering[A]) = {
    assert(ord.dimensions >= 1)
    new KDTree(KDTreeNode.buildTreeNode(0, points map {(_, true)}))
  }

  def newBuilder[A](implicit ord: DimensionalOrdering[A]): Builder[A, KDTree[A]] =
    new ArrayBuffer[A]() mapResult(x=>KDTree.fromSeq(x))

  implicit def canBuildFrom[B](implicit ordB: DimensionalOrdering[B]):
    CanBuildFrom[KDTree[_], B, KDTree[B]] = new CanBuildFrom[KDTree[_], B, KDTree[B]] {
      def apply: Builder[B, KDTree[B]] = newBuilder(ordB)
      def apply(from: KDTree[_]): Builder[B, KDTree[B]] = newBuilder(ordB)
  }
}

object KDTreeMap {
  def empty[A, B](implicit ord: DimensionalOrdering[A]): KDTreeMap[A, B] = KDTreeMap()

  def apply[A, B](points: (A, B)*)(implicit ord: DimensionalOrdering[A]) = fromSeq(points)

  def fromSeq[A, B](points: Seq[(A, B)])(implicit ord: DimensionalOrdering[A]) =
    new KDTreeMap(KDTreeNode.buildTreeNode(0, points))
}
