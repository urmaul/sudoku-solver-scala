package app.model

import app.TestCase
import app.model.GridKey.rowCol
import app.model.Solvers._
import eu.timepit.refined.auto._

class SolversTest extends TestCase {
  private def createGrid(cells: (GridKey, Digit)*): Grid =
    Grid.empty().set(cells).get

  "fillBox" should "work" in {
    val grid = createGrid(
      rowCol(1, 1) -> 2,
      rowCol(1, 4) -> 1,
      rowCol(2, 7) -> 1,
      rowCol(4, 1) -> 1,
      rowCol(7, 2) -> 1
    )

    assert(fillBox(1)(grid) == grid.set(rowCol(3, 3), 1).get)
  }

  "fillSimpleCases" should "work" in {
    val grid = createGrid(
      rowCol(1, 1) -> 2,
      rowCol(1, 4) -> 1,
      rowCol(2, 7) -> 1,
      rowCol(4, 1) -> 1,
      rowCol(7, 2) -> 1
    )

    assert(fillSimpleCases(grid) == grid.set(rowCol(3, 3), 1).get)
  }

  "fillRow" should "work" in {
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

    assert(fillRow(1)(grid) == grid.set(rowCol(1, 3), 7).get)
  }

  "fillCols" should "work" in {
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

    assert(fillCol(1)(grid) == grid.set(rowCol(3, 1), 7).get)
  }

  "fillSingleAlloweds" should "work" in {
    val grid = Grid.fromString("""
                                 |...1234..
                                 |.8.
                                 |..9
                                 |6
                                 |7
                                 |""".trim.stripMargin).get

    assert(fillSingleAlloweds(grid) == grid.set(rowCol(1, 1), 5).get)
  }
}
