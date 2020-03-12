package app.model

import eu.timepit.refined.auto._

import scala.annotation.tailrec

class Grid(val body: Vector[Cell]) {
  assert(body.size == 9 * 9)

  override def toString: String =
    GridKey
      .everyRow()
      .map(
        _.map(body(_).toString).mkString("")
      )
      .mkString("\n")

  override def equals(o: Any): Boolean = o match {
    case x: Grid => body.zip(x.body).iterator.forall((t) => t._1 == t._2)
    case _ => super.equals(o)
  }

  def tryDisallow(places: IterableOnce[GridKey], value: Digit): Grid = {
    val newBody = places.iterator.foldLeft(body)((b, i) =>
      body(i) match {
        case c: EmptyCell => b.updated(i, c.disallow(value))
        case _            => b
      })
    Grid(newBody)
  }
  def tryDisallow(place: GridKey, value: Digit): Grid = tryDisallow(List(place), value)

  def set(place: GridKey, value: Digit): Option[Grid] =
    Option.when(body(place).isAllowed(value)) {
      Grid(body.updated(place, FullCell(value))).tryDisallow(GridKey.affectedBy(place), value)
    }
  def set(cells: IterableOnce[(GridKey, Digit)]): Option[Grid] =
    cells.iterator.foldLeft[Option[Grid]](Some(this))((gridOption, kv) => gridOption.flatMap(_.set(kv._1, kv._2)))
  def set(cells: (GridKey, Digit)*): Option[Grid] = set(cells)
}
object Grid {
  def apply(body: Vector[Cell]): Grid = new Grid(body)
  def empty(): Grid = Grid(Vector.fill(9 * 9)(EmptyCell()))

  /**
   * Creates grid state from string.
   * @param string Expected string format is 9 lines 9 characters each.
   *               Chars with digits 1 to 9 are considered filled cells, any other char is considered empty cell.
   *               If a line has less than 9 chars or there is less than 9 lines, the rest is padded with empty cells.
   *               Chars and rows after 9th are ignored.
   * @return grid instance if we could create a valid grid.
   */
  def fromString(string: String): Option[Grid] = {
    val rowStrings = string.split("\n").padTo(9, "")
    val cells: List[(GridKey, Digit)] = allDigits.flatMap((row: Digit) => {
      val rowString = rowStrings(row.value - 1).padTo(9, '.')
      allDigits.flatMap((col: Digit) => {
        lazy val key = GridKey.rowCol(row, col)
        val char = rowString(col.value - 1)
        char match {
          case '1' => List((key, 1))
          case '2' => List((key, 2))
          case '3' => List((key, 3))
          case '4' => List((key, 4))
          case '5' => List((key, 5))
          case '6' => List((key, 6))
          case '7' => List((key, 7))
          case '8' => List((key, 8))
          case '9' => List((key, 9))
          case _ => List()
        }
      })
    })
    Grid.empty().set(cells)
  }

  /**
   * Takes 9 cells and searches for values that can be filled only in one of those cells
   * @param key a callback function to convert digit number to cell position
   * @param grid an input grid
   * @return a grid with cells filled if they could be
   */
  private def fillOnlyPossiblePosition(key: Digit => GridKey)(grid: Grid): Grid = {
    val emptyResult: Map[Digit, Set[Digit]] = Map.from(allDigits.map((_ -> Set[Digit]())))
    // allowedSets = Map(value -> [set of cells where this value is allowed])
    val allowedSets = allDigits.foldLeft(emptyResult)((map, num) => grid.body(key(num)) match {
      case FullCell(x) => map.removed(x.value)
      case EmptyCell(a) => a.foldLeft(map)((xmap, x) => xmap.updatedWith(x)(_.map(_ + num)))
    })
    allowedSets.foldLeft[Grid](grid)((grid, kv) => kv match {
      case (x, nums) => if (nums.size == 1) {
        grid.set(key(nums.head), x).getOrElse(grid)
      } else grid
    })
  }
  private def composeEveryDigit[A](f: Digit => A => A): A => A = allDigits.map(f).reduce(_ compose _)
  def fillBox(box: Digit): Grid => Grid = fillOnlyPossiblePosition(GridKey.boxNum(box, _))
  def fillBoxes: Grid => Grid = composeEveryDigit(fillBox)
  def fillRow(row: Digit): Grid => Grid = fillOnlyPossiblePosition(GridKey.rowCol(row, _))
  def fillRows: Grid => Grid = composeEveryDigit(fillRow)
  def fillCol(col: Digit): Grid => Grid = fillOnlyPossiblePosition(GridKey.rowCol(_, col))
  def fillCols: Grid => Grid = composeEveryDigit(fillCol)

  def fillSingleAlloweds(grid: Grid): Grid = {
    val cells: Seq[(GridKey, Digit)] = GridKey.every
      .map(i => grid.body(i) match {
        case EmptyCell(a) if a.size == 1 => Some(i -> a.head)
        case _ => None
      }).flatten
    grid.set(cells).getOrElse(grid)
  }

  @tailrec
  /** Run function f again and again while it produces updated result */
  private def repeatFilling(f: Grid => Grid)(grid: Grid): Grid = {
    val newGrid = f(grid)
    if (grid == newGrid) grid else repeatFilling(f)(newGrid)
  }

  def fillSimpleCases: Grid => Grid = repeatFilling(fillBoxes compose fillRows compose fillCols compose fillSingleAlloweds)

  /** Solve sudoku if possible */
  def solve(grid: Grid): Option[Grid] = {
    val newGrid = fillSimpleCases(grid)
    val firstEmpty: Option[(GridKey, Option[Digit])] = GridKey.every.zip(newGrid.body).collectFirst({
      case (key, EmptyCell(a)) => (key, a.headOption)
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