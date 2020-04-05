package app.model

import eu.timepit.refined.auto._

/** Index between 0 and 80 to define unique cell position on sudoku grid */
class GridKey private (val value: Int) {
  assert(value >= 0 && value < gridSize)
}
object GridKey {

  /** All the keys so we don't need to instantiate them every time. */
  val keys: Array[GridKey] = Array.range(0, gridSize).map(new GridKey(_))

  def rowCol(row: Digit, col: Digit): GridKey = keys((row - 1) * 9 + (col - 1))
  def boxNum(box: Digit, num: Digit): GridKey = keys(
    (box - 1) / 3 * 9 * 3 + // Box rows offset
      (box - 1) % 3 * 3 + // Box offset
      (num - 1) / 3 * 9 + // Number rows offset
      (num - 1) % 3 // Number offset
  )
  def wholeRow(row: Digit): Seq[GridKey] = allDigits.map(rowCol(row, _))
  def everyRow(): Seq[Seq[GridKey]] = allDigits.map(wholeRow)
  def wholeCol(col: Digit): Seq[GridKey] = allDigits.map(rowCol(_, col))
  def wholeBox(box: Digit): Seq[GridKey] = allDigits.map(boxNum(box, _))
  def rowOf(x: GridKey): Digit = allDigits(x.value / 9)
  def colOf(x: GridKey): Digit = allDigits(x.value % 9)
  def boxOf(x: GridKey): Digit =
    allDigits(x.value / 9 / 3 * 3 + x.value / 3 % 3)
  def wholeRowOf: GridKey => Seq[GridKey] = wholeRow _ compose rowOf
  def wholeColOf: GridKey => Seq[GridKey] = wholeCol _ compose colOf
  def wholeBoxOf: GridKey => Seq[GridKey] = wholeBox _ compose boxOf
  def affectedBy(x: GridKey): Set[GridKey] =
    (wholeRowOf(x) ++ wholeColOf(x) ++ wholeBoxOf(x)).toSet - x
}
