package com.thesamet.spatial

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class DimOrderingTest extends FlatSpec with ShouldMatchers {
  "dimOrderingFromTuple2" should "provide compareProjection that works" in {
    val dimOrd = DimensionalOrdering.dimOrderingFromTuple2[Int]
    dimOrd.dimensions should equal (2)
    dimOrd.compareProjection(0)((4, 7), (6, 3)) should be < (0)
    dimOrd.compareProjection(1)((4, 7), (6, 3)) should be > (0)
    dimOrd.compareProjection(0)((4, 7), (4, 3)) should be === (0)
    dimOrd.compareProjection(1)((4, 7), (4, 3)) should be > (0)
    dimOrd.compareProjection(1)((4, 7), (6, 7)) should be === (0)
  }

  "DimensionalOrdering.orderingBy" should "provide full ordering" in {
    val dimOrd = DimensionalOrdering.dimOrderingFromTuple2[Int]
    val ord0 = dimOrd.orderingBy(0)
    ord0.compare((4, 7), (6, 3)) should be < (0)
    ord0.compare((6, 7), (4, 3)) should be > (0)
    ord0.compare((4, 7), (4, 3)) should be > (0)
    ord0.compare((4, 3), (4, 7)) should be < (0)
    ord0.compare((4, 3), (4, 3)) should be === (0)

    val ord1 = dimOrd.orderingBy(1)
    ord1.compare((4, 7), (6, 3)) should be > (0)
    ord1.compare((6, 7), (4, 3)) should be > (0)
    ord1.compare((4, 3), (4, 6)) should be < (0)
    ord1.compare((3, 5), (4, 5)) should be < (0)
    ord1.compare((4, 3), (4, 5)) should be < (0)
    ord1.compare((4, 3), (4, 3)) should be === (0)
  }
}

class KDTreeTest extends FlatSpec with ShouldMatchers {
  "KDTree" should "builds an empty tree" in {
    val tree = KDTree.buildTree[(Int, Int)]()
    tree.size should equal (0)
    tree.findNearest((0, 0), 1) should equal (Nil)
    tree.findNearest((0, 0), 2) should equal (Nil)
  }

  it should "builds a tree with one point" in {
    val tree = KDTree.buildTree((0, 0))
    tree.size should equal (1)
    tree.findNearest((0, 0), 1) should equal (Seq((0, 0)))
    tree.findNearest((0, 0), 2) should equal (Seq((0, 0)))
    tree.findNearest((5, 4), 1) should equal (Seq((0, 0)))
  }

  it should "build a tree of 4 points" in {
    val points = Seq((3, 5), (9, 4), (17, 6), (18, 7))
    val tree = KDTree.buildTree(points: _*)
    tree.size should equal(4)
    tree.findNearest((3, 5), 1) should equal (Seq((3, 5)))
    tree.findNearest((9, 4), 1) should equal (Seq((9, 4)))
    tree.findNearest((17, 6), 1) should equal (Seq((17, 6)))
    tree.findNearest((18, 7), 1) should equal (Seq((18, 7)))

    tree.findNearest((3, 5), 2) should equal (Seq((3, 5), (9, 4)))
    tree.findNearest((9, 4), 2) should equal (Seq((9, 4), (3, 5)))
    tree.findNearest((6, 6), 3) should equal (Seq((3, 5), (9, 4), (17, 6)))
    tree.findNearest((6, 6), 4) should equal (Seq((3, 5), (9, 4), (17, 6), (18, 7)))
    tree.findNearest((6, 6), 5) should equal (Seq((3, 5), (9, 4), (17, 6), (18, 7)))
    tree.findNearest((6, 6), 12) should equal (Seq((3, 5), (9, 4), (17, 6), (18, 7)))
  }

  it should "build a large tree" in {
    val points = for { y <- 1 to 100; x <- 1 to 100 } yield (x, y)
    val t = KDTree.buildTree(points: _*)
    for { x <- 2 to (99, 10); y <- 2 to (99, 10) } {
      val near = t.findNearest((x, y), 9)
      near.size should equal (9)
      near.head should equal ((x, y))
      near.drop(1).take(4).toSet should equal (Set((x+1,y), (x-1,y), (x, y+1), (x, y-1)))
      near.drop(5).take(4).toSet should equal (Set((x-1,y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)))
    }

    t.findNearest((100, 1), 4).toSet should equal (
        Set((100, 1), (100, 2), (99, 1), (99, 2)))

    t.findNearest((100, 100), 4).toSet should equal (
        Set((100, 100), (99, 99), (100, 99), (99, 100)))

    t.findNearest((27, 0), 4).toSet should equal (
        Set((27, 1), (26, 1), (28, 1), (27, 2)))
  }
}

class KDTreeMapTest extends FlatSpec with ShouldMatchers {
  "KDTreeMap" should "work" in {
    val map = KDTreeMap.buildTreeMap(
      (3, 5) -> "a",
      (7, 6) -> "b",
      (12, 7) -> "c")
    map.size should equal (3)
    map.findNearest((3, 5), 1).toSet should equal (Set((3,5) -> "a"))
    map.findNearest((3, 5), 2).toSet should equal (Set((3,5) -> "a", (7,6) -> "b"))
    map.findNearest((7, 6), 2).toSet should equal (Set((3,5) -> "a", (7,6) -> "b"))
    map.findNearest((8, 6), 2).toSet should equal (Set((7,6) -> "b", (12,7) -> "c"))
  }
}
