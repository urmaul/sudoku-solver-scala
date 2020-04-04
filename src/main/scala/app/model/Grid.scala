package app.model

import eu.timepit.refined.auto._

class Grid private (val body: Vector[Cell]) {
  assert(body.size == gridSize)

  override def toString: String =
    body.grouped(9).map(_.mkString("")).mkString("\n")

  override def equals(o: Any): Boolean = o match {
    case x: Grid => body.zip(x.body).iterator.forall((t) => t._1 == t._2)
    case _       => super.equals(o)
  }

  def tryDisallow(places: IterableOnce[GridKey], value: Digit): Grid = {
    val newBody = places.iterator.foldLeft(body)((b, i) =>
      body(i) match {
        case c: EmptyCell => b.updated(i, c.disallow(value))
        case _            => b
    })
    Grid(newBody)
  }
  def tryDisallow(place: GridKey, value: Digit): Grid =
    tryDisallow(List(place), value)

  def set(place: GridKey, value: Digit): Option[Grid] =
    Option.when(body(place).isAllowed(value)) {
      Grid(body.updated(place, FullCell(value)))
        .tryDisallow(GridKey.affectedBy(place), value)
    }
  def set(cells: IterableOnce[(GridKey, Digit)]): Option[Grid] =
    cells.iterator.foldLeft[Option[Grid]](Some(this))((gridOption, kv) =>
      gridOption.flatMap(_.set(kv._1, kv._2)))
  def set(cells: (GridKey, Digit)*): Option[Grid] = set(cells)
}
object Grid {
  private def apply(body: Vector[Cell]): Grid = new Grid(body)
  def empty(): Grid = Grid(Vector.fill(gridSize)(EmptyCell()))

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
          case _   => List()
        }
      })
    })
    Grid.empty().set(cells)
  }
}
