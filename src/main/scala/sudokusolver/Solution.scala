package sudokusolver

import scala.annotation.tailrec
import scala.io.Source

object Solution {

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

    def valuesEliminated(square: Square, toEliminate: Set[Square]): Square = {
      val afterDefinite = definiteValuesEliminated(square, toEliminate)
      if (afterDefinite.size > 1)
        sharedValuesEliminated(afterDefinite, toEliminate)
      else afterDefinite
    }

    private def definiteValuesEliminated(square: Square, toEliminate: Set[Square]): Square = {
      def definiteValue(s: Square): Option[Int] =
        s.toList match {
          case head :: Nil => Some(head)
          case _           => None
        }
      square.diff(toEliminate.flatMap(definiteValue))
    }

    private def sharedValuesEliminated(square: Square, toEliminate: Set[Square]) = {
      val diffed = square.diff(toEliminate.flatten)
      if (diffed.isEmpty) square
      else diffed
    }
  }

  object Grid {

    def afterReducingSquare(grid: Grid, c: Coordinate): Grid =
      grid.zipWithIndex.map { case (rowSquares, rowIdx) =>
        rowSquares.zipWithIndex.map { case (square, colIdx) =>
          if (rowIdx == c._1 && colIdx == c._2) {
            val valuesInSameRow = Coordinate.inSameRow(c).map(c => grid(c._1)(c._2))
            val valuesInSameColumn = Coordinate.inSameColumn(c).map(c => grid(c._1)(c._2))
            val valuesInSameBox = Coordinate.inSameBox(c).map(c => grid(c._1)(c._2))
            Square.valuesEliminated(
              Square
                .valuesEliminated(Square.valuesEliminated(grid(c._1)(c._2), valuesInSameRow), valuesInSameColumn),
              valuesInSameBox
            )
          } else square
        }
      }
  }

  def iteration(grid: Grid): Grid = {
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

  def untilUnchanging(grid: Grid, f: Grid => Grid): Seq[Grid] = {
    @tailrec
    def go(acc: Seq[Grid]): Seq[Grid] = {
      val g = acc.last
      val n = f(g)
      if (n == g) acc
      else go(acc :+ n)
    }
    go(Seq(grid))
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

  def parsed(s: String): Seq[Int] =
    s.split(',').map(_.toInt)

  def parsed(src: Source): Seq[Seq[Int]] =
    src.getLines.toList.map(parsed)

  def toGrid(src: Source): Grid =
    parsed(src) map {
      _ map { value =>
        if (value == 0) (1 to 9).toSet
        else Set(value)
      }
    }

  def isSolved(grid: Grid): Boolean =
    grid forall { row =>
      row forall { square =>
        square.size == 1
      }
    }

  def solution(src: Source): Option[Grid] =
    untilUnchanging(toGrid(src), iteration).lastOption flatMap { state =>
      Some(state).filter(isSolved)
    }
}
