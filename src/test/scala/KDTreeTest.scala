package com.thesamet.spatial

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class DimOrderingTest extends FlatSpec with ShouldMatchers {
  "dimOrderingFromTuple2" should "provide compareProjection that works" in {
    val dimOrd = DimensionalOrdering.dimensionalOrderingForTuple2[Int]
    dimOrd.dimensions should equal (2)
    dimOrd.compareProjection(0)((4, 7), (6, 3)) should be < (0)
    dimOrd.compareProjection(1)((4, 7), (6, 3)) should be > (0)
    dimOrd.compareProjection(0)((4, 7), (4, 3)) should be === (0)
    dimOrd.compareProjection(1)((4, 7), (4, 3)) should be > (0)
    dimOrd.compareProjection(1)((4, 7), (6, 7)) should be === (0)
  }

  "DimensionalOrdering.orderingBy" should "provide full ordering" in {
    val dimOrd = DimensionalOrdering.dimensionalOrderingForTuple2[Int]
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

class NearestNeighborTest extends FlatSpec with ShouldMatchers {
  "KDTree" should "builds an empty tree" in {
    val tree = KDTree[(Int, Int)]()
    tree.size should equal (0)
    tree.findNearest((0, 0), 1) should equal (Nil)
    tree.findNearest((0, 0), 2) should equal (Nil)
    tree.contains((0,0)) should equal (false)
  }

  it should "builds a tree with one point" in {
    val tree = KDTree((0, 0))
    tree.size should equal (1)
    tree.findNearest((0, 0), 1) should equal (Seq((0, 0)))
    tree.findNearest((0, 0), 2) should equal (Seq((0, 0)))
    tree.findNearest((5, 4), 1) should equal (Seq((0, 0)))
    tree.contains((0,0)) should equal (true)
    tree.contains((1,0)) should equal (false)
  }

  it should "build a tree of 4 points" in {
    val points = Seq((3, 5), (9, 4), (17, 6), (18, 7))
    val tree = KDTree.fromSeq(points)
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
    for { pt <- points } {
      tree.contains(pt) should equal (true)
    }
    tree.contains((1,1)) should equal (false)
  }

  it should "build a large tree" in {
    val points = for { y <- 1 to 100; x <- 1 to 100 } yield (x, y)
    val t = KDTree.fromSeq(points)
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

class CollectionTest extends FlatSpec with ShouldMatchers {
  val points = Seq((3, 5), (9, 4), (17, 6), (18, 7))
  val tree = KDTree.fromSeq(points)

  "KDTree as Iterable" should "iterate over all elements" in {
    val s = tree.iterator.toList

    s should equal (List((9,4), (3,5), (17,6), (18,7)))
  }

  it should "be filterable" in {
    val s: KDTree[(Int, Int)] = tree.filter(_._1 > 5)
    s.size should equal (3)
  }

  it should "support map" in {
    val t: KDTree[(Int, Int)] = tree map {x => (x._1, x._2 + 5)}
    t.toList should equal (List((9,9), (3,10), (17,11), (18,12)))
  }

  it should "support map to a different dimensionality" in {
    val t: KDTree[(Int, Int, Int)] = tree map {x => (x._1, x._2 + 5, x._1 + x._2)}
    t.toList should equal (List((9,9,13), (3,10,8), (17,11,23), (18,12,25)))
  }
}

class KDTreeMapTest extends FlatSpec with ShouldMatchers {
  val map = KDTreeMap(
    (3, 5) -> "a",
    (7, 6) -> "b",
    (12, 7) -> "c")

  "KDTreeMap" should "work" in {
    map.size should equal (3)
    map.findNearest((3, 5), 1).toSet should equal (Set((3,5) -> "a"))
    map.findNearest((3, 5), 2).toSet should equal (Set((3,5) -> "a", (7,6) -> "b"))
    map.findNearest((7, 6), 2).toSet should equal (Set((3,5) -> "a", (7,6) -> "b"))
    map.findNearest((8, 6), 2).toSet should equal (Set((7,6) -> "b", (12,7) -> "c"))
  }

  it should "support get" in {
    map.get((3, 5)) should equal (Some("a"))
    map.get((7, 6)) should equal (Some("b"))
    map.get((17, 9)) should equal (None)
  }

  it should "support +" in {
    val newMap: KDTreeMap[(Int, Int), String] = map + ((17, 9) -> "d")
    newMap.get((3, 5)) should equal (Some("a"))
    newMap.get((7, 6)) should equal (Some("b"))
    newMap.get((17, 9)) should equal (Some("d"))
  }

  it should "support -" in {
    val newMap: KDTreeMap[(Int, Int), String] = map - ((7, 6))
    newMap.get((3, 5)) should equal (Some("a"))
    newMap.get((7, 6)) should equal (None)
    newMap.get((12, 7)) should equal (Some("c"))
  }
}
