package app

import eu.timepit.refined.auto._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval

package object model {
  type Digit = Int Refined Interval.Closed[1, 9]
  val allDigits: List[Digit] = List[Digit](1, 2, 3, 4, 5, 6, 7, 8, 9)
  var gridSize: Int = 9 * 9

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
}
