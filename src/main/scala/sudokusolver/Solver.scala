package sudokusolver

import scala.annotation.tailrec

object Solver extends App {

  type Coordinate = (Int, Int)
  type Square = Set[Int]
  type Grid = Seq[Seq[Square]]

  val givenState = Seq(
    Seq(0, 0, 8, 2, 0, 6, 1, 0, 9),
    Seq(1, 0, 0, 0, 3, 8, 0, 0, 0),
    Seq(0, 0, 6, 0, 0, 0, 3, 2, 0),
    Seq(0, 0, 0, 5, 7, 9, 8, 0, 2),
    Seq(2, 8, 0, 4, 0, 3, 0, 6, 5),
    Seq(5, 0, 7, 8, 6, 2, 0, 0, 0),
    Seq(0, 6, 5, 0, 0, 0, 9, 0, 0),
    Seq(0, 0, 0, 6, 8, 0, 0, 0, 3),
    Seq(7, 0, 3, 9, 0, 1, 2, 0, 0),
  )

  val grid: Grid =
    givenState.map {
      _.map { value =>
        if (value == 0) (1 to 9).toSet
        else Set(value)
      }
    }

  object Coordinate {

    def inSameRow(c: Coordinate): Set[Coordinate] =
      ((0 until c._2) ++ ((c._2 + 1) to 8))
        .foldLeft(Set[Coordinate]()) { (acc, col) =>
          acc + ((c._1, col))
        }

    def inSameColumn(c: Coordinate): Set[Coordinate] =
      ((0 until c._1) ++ ((c._1 + 1) to 8))
        .foldLeft(Set[Coordinate]()) { (acc, row) =>
          acc + ((row, c._2))
        }

    def boxRange(i: Int): Range = {
      val idx = (i / 3) * 3
      idx to idx + 2
    }

    def boxRowRange(c: Coordinate): Range = boxRange(c._1)

    def boxColumnRange(c: Coordinate): Range = boxRange(c._2)

    def inSameBox(c: Coordinate): Set[Coordinate] =
      boxColumnRange(c).flatMap { col =>
        boxRowRange(c) map { row =>
          (row, col)
        }
      }.toSet - c

    def toCheck(c: Coordinate): Set[Coordinate] =
      inSameRow(c) ++ inSameColumn(c) ++ inSameBox(c)
  }

  object Square {

    def squaresEliminated(square: Square, toEliminate: Set[Square]): Square = {
      def definiteValue(s: Square): Option[Int] = s.toList match {
        case head :: Nil => Some(head)
        case _           => None
      }
      valuesEliminated(square, toEliminate.flatMap(definiteValue))
    }

    def valuesEliminated(square: Square, toEliminate: Set[Int]): Set[Int] =
      square.diff(toEliminate)
  }

  object Grid {

    def afterReducingSquare(grid: Grid, c: Coordinate): Grid =
      grid.zipWithIndex.map {
        case (rowSquares, rowIdx) =>
          rowSquares.zipWithIndex.map {
            case (square, colIdx) =>
              if (rowIdx == c._1 && colIdx == c._2) {
                val squaresToCheck =
                  Coordinate.toCheck(c).map(c => grid(c._1)(c._2))
                Square.squaresEliminated(grid(c._1)(c._2), squaresToCheck)
              } else square
          }
      }
  }

  def pass(grid: Grid): Grid = {
    @tailrec
    def go(g: Grid, c: Coordinate): Grid = {
      val gridAfter = Grid.afterReducingSquare(g, c)
      if (c._1 == 8 && c._2 == 8) gridAfter
      else {
        val row = if (c._1 < 8) c._1 + 1 else 0
        val col = if (row == 0) c._2 + 1 else c._2
        go(gridAfter, (row, col))
      }
    }
    go(grid, (0, 0))
  }

  def passUntilUnchanging(grid: Grid): Grid = {
    @tailrec
    def go(h: Grid): Grid = {
      val n = pass(h)
      if (n == h) h
      else go(n)
    }
    go(grid)
  }

  def asString(g: Grid): String = {
    def asString(row: Seq[Square]): String = {
      def asString(square: Square): String = {
        square.toList match {
          case hd :: Nil => hd.toString
          case _         => square.toList.sorted.toString
        }
      }
      row.map(asString).mkString(" ")
    }
    g.map(asString).mkString("\n")
  }

  println(asString(grid))
  println
  println(asString(passUntilUnchanging(grid)))
}
