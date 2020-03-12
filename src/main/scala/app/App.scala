package app

import app.model._

object App {
  def main(args: Array[String]): Unit = {
    val grid: Grid = Grid.fromString(
      """
        |.1..7...5
        |6...5..4.
        |.5..3....
        |..8..6..2
        |........7
        |..91.3...
        |.........
        |..6..41..
        |8....53.4
        |""".trim.stripMargin).get

    println("--------- Before:")
    println(grid)
    println("--------- After:")
    println(Grid.solve(grid).get)
  }
}
