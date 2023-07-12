package sudokusolver

object Problem {

  val givenState = Seq(
    Seq(0, 0, 8, 2, 0, 6, 1, 0, 9),
    Seq(1, 0, 0, 0, 3, 8, 0, 0, 0),
    Seq(0, 0, 6, 0, 0, 0, 3, 2, 0),
    Seq(0, 0, 0, 5, 7, 9, 8, 0, 2),
    Seq(2, 8, 0, 4, 0, 3, 0, 6, 5),
    Seq(5, 0, 7, 8, 6, 2, 0, 0, 0),
    Seq(0, 6, 5, 0, 0, 0, 9, 0, 0),
    Seq(0, 0, 0, 6, 8, 0, 0, 0, 3),
    Seq(7, 0, 3, 9, 0, 1, 2, 0, 0)
  )

  val grid: Grid =
    givenState.map {
      _.map { value =>
        if (value == 0) (1 to 9).toSet
        else Set(value)
      }
    }
}
