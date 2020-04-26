package sudokusolver

import org.scalatest.funsuite.AnyFunSuite
import sudokusolver.Solution.{Coordinate, Square}

import scala.io.Source

class SolutionTest extends AnyFunSuite {

  test("valuesEliminated leaves definite value alone") {
    val square = Set(2)
    val toEliminate = Set(
      Set(1),
      Set(3),
      Set(4),
      Set(5),
      Set(6),
      Set(8),
      Set(9)
    )
    assertResult(Set(2))(Square.valuesEliminated(square, toEliminate))
  }

  test("valuesEliminated leaves only possible value") {
    val square = Set(3, 7, 8)
    val toEliminate = Set(
      Set(5),
      Set(3, 4, 6),
      Set(4, 6, 7),
      Set(2),
      Set(6, 7),
      Set(9),
      Set(1, 7, 3),
      Set(3, 4, 7)
    )
    assertResult(Set(8))(Square.valuesEliminated(square, toEliminate))
  }

  test("valuesEliminated leaves definite value alone comparing with multiple values") {
    val square = Set(2)
    val toEliminate = Set(
      Set(1, 2, 3, 4, 5, 6, 7, 8, 9),
      Set(1),
      Set(3),
      Set(4),
      Set(5),
      Set(6),
      Set(8),
      Set(9)
    )
    assertResult(Set(2))(Square.valuesEliminated(square, toEliminate))
  }

  test("valuesEliminated compares multiple values") {
    val square = Set(1, 3, 7)
    val toEliminate = Set(
      Set(5),
      Set(3, 4, 6),
      Set(8),
      Set(4, 6, 7),
      Set(2),
      Set(6, 7),
      Set(9),
      Set(3, 4, 7)
    )
    assertResult(Set(1))(Square.valuesEliminated(square, toEliminate))
  }

  test("valuesEliminated returns original when nothing left") {
    val square = Set(6, 7)
    val toEliminate = Set(
      Set(6, 7)
    )
    assertResult(Set(6, 7))(Square.valuesEliminated(square, toEliminate))
  }

  test("Coordinates in same row are correct") {
    assertResult(Set((0, 0), (0, 1), (0, 2), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8)))(Coordinate.inSameRow((0, 3)))
  }

  test("Coordinates in same column are correct") {
    assertResult(Set((1, 3), (2, 3), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3)))(Coordinate.inSameColumn((0, 3)))
  }

  test("Coordinates in same box are correct") {
    assertResult(Set((0, 4), (0, 5), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5)))(Coordinate.inSameBox((0, 3)))
  }

  test("Row range of box is correct") {
    assertResult(0 to 2)(Coordinate.boxRowRange(0, 3))
  }

  test("Column range of box is correct") {
    assertResult(3 to 5)(Coordinate.boxColumnRange(0, 3))
  }

  // https://sudoku.com/easy
  test("easy1 has a solution") {
    //val source = Source.fromURL(getClass.getResource("/easy1.csv"))
    val source = Source.fromFile("src/test/resources/easy1.csv")
    val solution = Solution.solution(source)
    assert(solution.isDefined)
  }

  // https://sudoku.com/medium
  test("medium1 has a solution") {
    //val source = Source.fromURL(getClass.getResource("/easy1.csv"))
    val source = Source.fromFile("src/test/resources/medium1.csv")
    val solution = Solution.solution(source)
    assert(solution.isDefined)
  }
}
