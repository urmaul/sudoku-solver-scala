package app.model

import app.TestCase
import eu.timepit.refined.auto._

class GridTest extends TestCase {
  "Grid.set" should "disallow setting same digit in same row, column or box" in {
    forAll { (value: Digit, row: Digit, col: Digit, x: Digit) =>
      val grid = Grid.empty().set(GridKey.rowCol(row, col), value)
      assert(
        col == x || grid.flatMap(_.set(GridKey.rowCol(row, x), value)).isEmpty)
      assert(
        row == x || grid.flatMap(_.set(GridKey.rowCol(x, col), value)).isEmpty)
    }
  }

  it should "disallow setting same digit in same box" in {
    forAll { (value: Digit, box: Digit, num: Digit, x: Digit) =>
      val grid = Grid.empty().set(GridKey.boxNum(box, num), value)
      assert(
        num == x || grid.flatMap(_.set(GridKey.boxNum(box, x), value)).isEmpty)
    }
  }

  it should "allow setting another digit in same row or column" in {
    forAll {
      (value: Digit, newValue: Digit, row: Digit, col: Digit, x: Digit) =>
        val grid = Grid.empty().set(GridKey.rowCol(row, col), value)
        assert(
          col == x || newValue == value || grid
            .flatMap(_.set(GridKey.rowCol(row, x), newValue))
            .isDefined)
        assert(
          row == x || newValue == value || grid
            .flatMap(_.set(GridKey.rowCol(x, col), newValue))
            .isDefined)
    }
  }

  it should "allow setting another digit in same box" in {
    forAll {
      (value: Digit, newValue: Digit, box: Digit, num: Digit, x: Digit) =>
        val grid = Grid.empty().set(GridKey.boxNum(box, num), value)
        assert(
          num == x || newValue == value || grid
            .flatMap(_.set(GridKey.boxNum(box, x), newValue))
            .isDefined)
    }
  }

  "Grid.fillBox" should "work" in {
    val grid = Grid
      .empty()
      .set(
        GridKey.rowCol(1, 1) -> 2,
        GridKey.rowCol(1, 4) -> 1,
        GridKey.rowCol(2, 7) -> 1,
        GridKey.rowCol(4, 1) -> 1,
        GridKey.rowCol(7, 2) -> 1
      )

    assert(
      grid.map(Grid.fillBox(1)) == grid.flatMap(_.set(GridKey.rowCol(3, 3), 1)))
  }

  "Grid.fillSimpleCases" should "work" in {
    val grid = Grid
      .empty()
      .set(
        GridKey.rowCol(1, 1) -> 2,
        GridKey.rowCol(1, 4) -> 1,
        GridKey.rowCol(2, 7) -> 1,
        GridKey.rowCol(4, 1) -> 1,
        GridKey.rowCol(7, 2) -> 1
      )

    assert(
      grid.map(Grid.fillSimpleCases) == grid.flatMap(
        _.set(GridKey.rowCol(3, 3), 1)))
  }

  "Grid.fillRow" should "work" in {
    val grid = Grid.fromString("""
        |...123456
        |...
        |...
        |7..
        |...
        |...
        |.7.
        |...
        |...
        |""".trim.stripMargin).get

    assert(Grid.fillRow(1)(grid) == grid.set(GridKey.rowCol(1, 3), 7).get)
  }

  "Grid.fillCols" should "work" in {
    val grid = Grid.fromString("""
        |...7
        |......7
        |...
        |1
        |2
        |3
        |4
        |5
        |6
        |""".trim.stripMargin).get

    assert(Grid.fillCol(1)(grid) == grid.set(GridKey.rowCol(3, 1), 7).get)
  }

  "Grid.fillSingleAlloweds" should "work" in {
    val grid = Grid.fromString("""
        |...1234..
        |.8.
        |..9
        |6
        |7
        |""".trim.stripMargin).get

    assert(
      Grid.fillSingleAlloweds(grid) == grid.set(GridKey.rowCol(1, 1), 5).get)
  }

  "Grid.fromString" should "work" in {
    val expected = Grid
      .empty()
      .set(
        GridKey.rowCol(1, 1) -> 2,
        GridKey.rowCol(1, 4) -> 1,
        GridKey.rowCol(2, 7) -> 3,
        GridKey.rowCol(4, 1) -> 4,
        GridKey.rowCol(7, 2) -> 5
      )
      .get
    val grid = Grid.fromString("""
        |2..1
        |......3
        |
        |4
        |
        |
        |.5
        |""".trim.stripMargin).get

    assert(grid == expected)
  }

  "Grid.fromString" should "convert back from toString" in {
    val expected = Grid
      .empty()
      .set(
        GridKey.rowCol(1, 1) -> 2,
        GridKey.rowCol(1, 4) -> 1,
        GridKey.rowCol(2, 7) -> 3,
        GridKey.rowCol(4, 1) -> 4,
        GridKey.rowCol(7, 2) -> 5
      )
      .get
    val grid = Grid.fromString(expected.toString).get

    assert(grid == expected)
  }
}
