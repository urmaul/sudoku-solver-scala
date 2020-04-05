package app.model

import app.TestCase
import eu.timepit.refined.auto._

class GridKeyTest extends TestCase {
  "keys" should "have values according to indexes" in {
    assert(GridKey.keys.size == gridSize)
    GridKey.keys.zipWithIndex.foreach({
      case (key, index) => assert(key.value == index)
    })
  }

  "rowCol" should "return a valid value" in {
    forAll { (row: Digit, col: Digit) =>
      GridKey.rowCol(row, col)
    }
  }

  "boxNum" should "return a valid value" in {
    forAll { (box: Digit, num: Digit) =>
      GridKey.boxNum(box, num)
    }
  }

  it should "get coordinates in right position" in {
    val boxes = Vector[Digit](
      1, 1, 1, 2, 2, 2, 3, 3, 3,
      1, 1, 1, 2, 2, 2, 3, 3, 3,
      1, 1, 1, 2, 2, 2, 3, 3, 3,
      4, 4, 4, 5, 5, 5, 6, 6, 6,
      4, 4, 4, 5, 5, 5, 6, 6, 6,
      4, 4, 4, 5, 5, 5, 6, 6, 6,
      7, 7, 7, 8, 8, 8, 9, 9, 9,
      7, 7, 7, 8, 8, 8, 9, 9, 9,
      7, 7, 7, 8, 8, 8, 9, 9, 9,
    )
    val numbers = Vector[Digit](
      1, 2, 3, 1, 2, 3, 1, 2, 3,
      4, 5, 6, 4, 5, 6, 4, 5, 6,
      7, 8, 9, 7, 8, 9, 7, 8, 9,
      1, 2, 3, 1, 2, 3, 1, 2, 3,
      4, 5, 6, 4, 5, 6, 4, 5, 6,
      7, 8, 9, 7, 8, 9, 7, 8, 9,
      1, 2, 3, 1, 2, 3, 1, 2, 3,
      4, 5, 6, 4, 5, 6, 4, 5, 6,
      7, 8, 9, 7, 8, 9, 7, 8, 9,
    )

    forAll { (box: Digit, num: Digit) =>
      val key = GridKey.boxNum(box, num)
      assert((boxes(key.value), numbers(key.value)) == (box, num))
    }
  }

  "colOf and rowOf" should "work" in {
    forAll { (col: Digit, row: Digit) =>
      val key = GridKey.rowCol(row, col)
      assert((GridKey.rowOf(key), GridKey.colOf(key)) == (row, col))
    }
  }

  "boxOf" should "work" in {
    forAll { (box: Digit, num: Digit) =>
      val key = GridKey.boxNum(box, num)
      assert(GridKey.boxOf(key) == box)
    }
  }

}
