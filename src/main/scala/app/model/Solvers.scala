package app.model

import app.model.GridKey.{boxNum, rowCol}

import scala.annotation.tailrec

object Solvers {
  private type BasicSolver = Grid => Grid;

  /**
    * Takes 9 cells and searches for values that can be filled only in one of those cells
    * @param key a callback function to convert digit number to cell position
    */
  private def fillOnlyPossiblePosition(key: Digit => GridKey): BasicSolver =
    (grid) => {
      val emptyResult: Map[Digit, Set[Digit]] =
        Map.from(allDigits.map((_ -> Set[Digit]())))
      // allowedSets = Map(value -> [set of cells where this value is allowed])
      val allowedSets = allDigits.foldLeft(emptyResult)((map, num) =>
        grid.body(key(num).value) match {
          case FullCell(x) => map.removed(x)
          case EmptyCell(a) =>
            a.foldLeft(map)((xmap, x) => xmap.updatedWith(x)(_.map(_ + num)))
      })
      val singlePositions: Iterable[(GridKey, Digit)] = allowedSets
        .map({
          case (x, nums) if nums.size == 1 => Some(key(nums.head) -> x)
          case _                           => None
        })
        .flatten
      grid.set(singlePositions).getOrElse(grid)
    }
  def fillBox(box: Digit): BasicSolver =
    fillOnlyPossiblePosition(boxNum(box, _))
  def fillRow(row: Digit): BasicSolver =
    fillOnlyPossiblePosition(rowCol(row, _))
  def fillCol(col: Digit): BasicSolver =
    fillOnlyPossiblePosition(rowCol(_, col))

  private def forEveryDigit(f: Digit => BasicSolver): BasicSolver =
    allDigits.map(f).reduce(_ compose _)

  def fillSingleAlloweds(grid: Grid): Grid = {
    val cells: Seq[(GridKey, Digit)] = grid.body.zip(GridKey.keys)
      .map({
        case (EmptyCell(a), i) if a.size == 1 => Some(i -> a.head)
        case _                                => None
      })
      .flatten
    grid.set(cells).getOrElse(grid)
  }

  @tailrec
  /** Run function f again and again while it produces updated result */
  private def repeatFilling(f: BasicSolver)(grid: Grid): Grid = {
    val newGrid = f(grid)
    if (grid == newGrid) grid else repeatFilling(f)(newGrid)
  }

  lazy val fillSimpleCases: BasicSolver =
    repeatFilling(
      forEveryDigit(fillBox) compose
        forEveryDigit(fillRow) compose
        forEveryDigit(fillCol) compose
        fillSingleAlloweds
    )

  /** Solve sudoku if possible */
  def solve(grid: Grid): Option[Grid] = {
    val newGrid = fillSimpleCases(grid)
    val firstEmpty: Option[(GridKey, Option[Digit])] = newGrid.body.zip(GridKey.keys)
      .collectFirst({
        case (EmptyCell(a), key) => (key, a.headOption)
      })
    firstEmpty match {
      // No empty cells found => grid is solved
      case None => Some(newGrid)
      // Empty cell found but no value is allowed in it => grid is not solvable
      case Some(_ -> None) => None
      // Empty cell found
      case Some(key -> Some(value)) =>
        // Fill first found empty cell with allowed value. Try solving result.
        // If result is unsolvable, we are sure this value cannot be in this cell.
        solve(newGrid.set(key, value).getOrElse(newGrid))
          .orElse(solve(newGrid.tryDisallow(key, value)))
    }
  }
}
