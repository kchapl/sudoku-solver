package object sudokusolver {
  type Coordinate = (Int, Int)
  type Square = Set[Int]
  type Grid = Seq[Seq[Square]]
}
