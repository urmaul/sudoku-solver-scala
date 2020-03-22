package app

import eu.timepit.refined.auto._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval

package object model {
  type Digit = Int Refined Interval.Closed[1, 9]
  val allDigits: List[Digit] = List[Digit](1, 2, 3, 4, 5, 6, 7, 8, 9)

  sealed trait Cell {
    def isValid: Boolean = this match {
      case _: FullCell  => true
      case EmptyCell(x) => x.nonEmpty
    }

    /** Returns true if it is allowed to set value to this cell */
    def isAllowed(value: Digit): Boolean = this match {
      case _: FullCell  => false
      case EmptyCell(x) => x.contains(value)
    }

    def isEmpty: Boolean = this match {
      case _: EmptyCell => true
      case _: FullCell  => false
    }

    override def toString: String = this match {
      case FullCell(x)  => x.toString
      case _: EmptyCell => "."
    }
  }
  case class FullCell(value: Digit) extends Cell
  case class EmptyCell(allowed: Set[Digit]) extends Cell {
    def disallow(value: Digit): EmptyCell = EmptyCell(allowed - value)
  }

  object EmptyCell {
    private lazy val fullAllowed: EmptyCell = new EmptyCell(
      allDigits.toSet[Digit])

    def apply(allowed: Set[Digit]): EmptyCell = new EmptyCell(allowed)
    def apply(): EmptyCell = fullAllowed
  }

  /** Integer number between 0 and 80 to define unique cell position on sudoku grid */
  type GridKey = Int
  object GridKey {
    def isValid(x: GridKey): Boolean = x >= 0 && x < 9 * 9

    def every: Seq[GridKey] = (0 until 9 * 9)
    def rowCol(row: Digit, col: Digit): GridKey = (row - 1) * 9 + (col - 1)
    def boxNum(box: Digit, num: Digit): GridKey =
      (box - 1) / 3 * 9 * 3 + // Box rows offset
        (box - 1) % 3 * 3 + // Box offset
        (num - 1) / 3 * 9 + // Number rows offset
        (num - 1) % 3 // Number offset
    def wholeRow(row: Digit): Seq[GridKey] = allDigits.map(rowCol(row, _))
    def everyRow(): Seq[Seq[GridKey]] = allDigits.map(wholeRow)
    def wholeCol(col: Digit): Seq[GridKey] = allDigits.map(rowCol(_, col))
    def wholeBox(box: Digit): Seq[GridKey] = allDigits.map(boxNum(box, _))
    def rowOf(x: GridKey): Digit = { require(isValid(x)); allDigits(x / 9) }
    def colOf(x: GridKey): Digit = { require(isValid(x)); allDigits(x % 9) }
    def boxOf(x: GridKey): Digit = {
      require(isValid(x)); allDigits(x / 9 / 3 * 3 + x / 3 % 3)
    }
    def wholeRowOf: GridKey => Seq[GridKey] = wholeRow _ compose rowOf
    def wholeColOf: GridKey => Seq[GridKey] = wholeCol _ compose colOf
    def wholeBoxOf: GridKey => Seq[GridKey] = wholeBox _ compose boxOf
    def affectedBy(x: GridKey): Set[GridKey] = {
      require(isValid(x))
      (wholeRowOf(x) ++ wholeColOf(x) ++ wholeBoxOf(x)).toSet - x
    }
  }
}
