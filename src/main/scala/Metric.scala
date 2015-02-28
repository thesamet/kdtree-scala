package com.thesamet.spatial

import scala.language.implicitConversions
import scala.math.Numeric.Implicits._

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
  implicit def metricFromTuple2[A](implicit n: Numeric[A]): Metric[(A, A), A] = new Metric[(A, A), A] {
    def distance(x: (A, A), y: (A, A)): A = {
      val dx = x._1 - y._1
      val dy = x._2 - y._2
      dx * dx + dy * dy
    }
    def planarDistance(d: Int)(x: (A, A), y: (A, A)): A = {
      val dd = x.productElement(d).asInstanceOf[A] - y.productElement(d).asInstanceOf[A]
      dd * dd
    }
  }
}
