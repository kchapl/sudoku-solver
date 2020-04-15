package sudokusolver

import sudokusolver.Problem.grid
import sudokusolver.Solution.{asString, iteration, untilUnchanging}

object Main extends App {
  val grids = untilUnchanging(grid, iteration)
  for (g <- grids) {
    println
    println(asString(g))
  }
  println
  println(s"took ${grids.length - 1} iterations")
}
