package com.thesamet.spatial

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class RegionTest extends FlatSpec with ShouldMatchers {
  def sandwitch(a: Int, b: Int) =
    RegionIntersection(Seq(AboveHyperplane((3, a), 1), BelowHyperplane((3, b), 1)))

  "overlapsWith" should "work correctly for AboveHyperplane and BelowHyperplane" in {
    // on same dimension
    AboveHyperplane((3, 5), 1).overlapsWith(AboveHyperplane((3, 4), 1)) should equal (true)
    AboveHyperplane((3, 5), 1).overlapsWith(AboveHyperplane((3, 5), 1)) should equal (true)
    AboveHyperplane((3, 5), 1).overlapsWith(AboveHyperplane((3, 6), 1)) should equal (true)

    AboveHyperplane((3, 5), 1).overlapsWith(BelowHyperplane((3, 4), 1)) should equal (false)
    AboveHyperplane((3, 5), 1).overlapsWith(BelowHyperplane((3, 5), 1)) should equal (true)
    AboveHyperplane((3, 5), 1).overlapsWith(BelowHyperplane((3, 6), 1)) should equal (true)

    BelowHyperplane((3, 5), 1).overlapsWith(AboveHyperplane((3, 4), 1)) should equal (true)
    BelowHyperplane((3, 5), 1).overlapsWith(AboveHyperplane((3, 5), 1)) should equal (true)
    BelowHyperplane((3, 5), 1).overlapsWith(AboveHyperplane((3, 6), 1)) should equal (false)

    BelowHyperplane((3, 5), 1).overlapsWith(BelowHyperplane((3, 4), 1)) should equal (true)
    BelowHyperplane((3, 5), 1).overlapsWith(BelowHyperplane((3, 5), 1)) should equal (true)
    BelowHyperplane((3, 5), 1).overlapsWith(BelowHyperplane((3, 6), 1)) should equal (true)

    // on different dimensions should always be true
    for {x <- 3 to 5; y <- 3 to 5; i <- 3 to 5; j <- 3 to 5} {
      AboveHyperplane((x, y), 0).overlapsWith(AboveHyperplane((i, j), 1)) should equal (true)
      BelowHyperplane((x, y), 0).overlapsWith(AboveHyperplane((i, j), 1)) should equal (true)
      AboveHyperplane((x, y), 0).overlapsWith(BelowHyperplane((i, j), 1)) should equal (true)
      BelowHyperplane((x, y), 0).overlapsWith(BelowHyperplane((i, j), 1)) should equal (true)
    }
  }

  it should "work correctly for RegionIntersections" in {
    sandwitch(3, 5).overlapsWith(AboveHyperplane((3, 7), 1)) should equal (false)
    sandwitch(3, 5).overlapsWith(BelowHyperplane((3, 7), 1)) should equal (true)
    sandwitch(3, 5).overlapsWith(AboveHyperplane((3, 2), 1)) should equal (true)
    sandwitch(3, 5).overlapsWith(AboveHyperplane((3, 3), 1)) should equal (true)
    sandwitch(3, 5).overlapsWith(BelowHyperplane((3, 2), 1)) should equal (false)

    sandwitch(3, 5).overlapsWith(sandwitch(6, 10)) should equal (false)
    sandwitch(3, 5).overlapsWith(sandwitch(1, 2)) should equal (false)
    sandwitch(3, 5).overlapsWith(sandwitch(4, 7)) should equal (true)
    sandwitch(3, 5).overlapsWith(sandwitch(1, 4)) should equal (true)
  }

  "contains" should "work for all region types" in {
    AboveHyperplane((3, 5), 1).contains((1, 5)) should equal (true)
    AboveHyperplane((3, 5), 1).contains((9, 4)) should equal (false)
    AboveHyperplane((3, 5), 1).contains((9, 7)) should equal (true)
    BelowHyperplane((3, 5), 1).contains((9, 4)) should equal (true)
    BelowHyperplane((3, 5), 1).contains((9, 7)) should equal (false)

    sandwitch(3, 5).contains((7,4)) should equal (true)
    sandwitch(3, 5).contains((7,7)) should equal (false)
  }

  "RegionBuilder" should "build regions corrections" in {
    new RegionBuilder[(Int, Int)] {
      from((3, 5), 1)
    }.build should equal(AboveHyperplane((3, 5), 1))

    new RegionBuilder[(Int, Int)] {
      to((3, 5), 1)
    }.build should equal(BelowHyperplane((3, 5), 1))

    new RegionBuilder[(Int, Int)] {
    }.build should equal(EntireSpace())

    new RegionBuilder[(Int, Int)] {
      from((3, 3), 1)
      to((3, 5), 1)
    }.build should equal(sandwitch(3, 5))
  }

  "Region" should "allow dotless syntax" in {
    (Region from((35, 0), 0) to((43, 0), 0) from((0, 81), 1) to((0, 84), 1) build) should equal(
      RegionIntersection(Seq(
        AboveHyperplane((35, 0), 0),
        BelowHyperplane((43, 0), 0),
        AboveHyperplane((0, 81), 1),
        BelowHyperplane((0, 84), 1)
      )))
  }
}
