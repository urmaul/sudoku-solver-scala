package app.model

import app.TestCase
import app.model.GridKey._
import eu.timepit.refined.auto._

class GridTest extends TestCase {
  private def createGrid(cells: (GridKey, Digit)*): Grid =
    Grid.empty().set(cells).get

  "Grid.set" should "disallow setting same digit in same row, column or box" in {
    forAll { (value: Digit, row: Digit, col: Digit, x: Digit) =>
      val grid = createGrid(rowCol(row, col) -> value)
      assert(grid.set(rowCol(row, x), value).isEmpty)
      assert(grid.set(rowCol(x, col), value).isEmpty)
    }
  }

  it should "disallow setting same digit in same box" in {
    forAll { (value: Digit, box: Digit, nums: TwoDifferentDigits) =>
      val grid = createGrid(boxNum(box, nums.a) -> value)
      assert(grid.set(boxNum(box, nums.b), value).isEmpty)
    }
  }

  it should "allow setting another digit in same row" in {
    forAll {
      (values: TwoDifferentDigits, row: Digit, cols: TwoDifferentDigits) => {
        val grid = createGrid(rowCol(row, cols.a) -> values.a)
        assert(grid.set(rowCol(row, cols.b), values.b).isDefined)
      }
    }
  }

  it should "allow setting another digit in same column" in {
    forAll {
      (values: TwoDifferentDigits, rows: TwoDifferentDigits, col: Digit) => {
        val grid = createGrid(rowCol(rows.a, col) -> values.a)
        assert(grid.set(rowCol(rows.b, col), values.b).isDefined)
      }
    }
  }

  it should "allow setting another digit in same box" in {
    forAll {
      (values: TwoDifferentDigits, box: Digit, nums: TwoDifferentDigits) =>
        val grid = createGrid(boxNum(box, nums.a) -> values.a)
        assert(grid.set(boxNum(box, nums.b), values.b).isDefined)
    }
  }

  "Grid.fromString" should "work" in {
    val expected = createGrid(
      rowCol(1, 1) -> 2,
      rowCol(1, 4) -> 1,
      rowCol(2, 7) -> 3,
      rowCol(4, 1) -> 4,
      rowCol(7, 2) -> 5
    )
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
    val expected = createGrid(
      rowCol(1, 1) -> 2,
      rowCol(1, 4) -> 1,
      rowCol(2, 7) -> 3,
      rowCol(4, 1) -> 4,
      rowCol(7, 2) -> 5
    )
    val grid = Grid.fromString(expected.toString).get

    assert(grid == expected)
  }
}
