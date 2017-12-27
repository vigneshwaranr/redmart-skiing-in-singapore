package in.vigneshwaran

package object skiing {
  type Map = Array[Array[Int]]
  type Memo = Array[Array[HighestElevInfo]]
  type Length = Int
  type Drop = Int
  type Elev = Int
  type HighestElev = Int
  type RowIdx = Int
  type ColIdx = Int

  type HighestElevInfo = (Length, HighestElev)
  type Position = (RowIdx, ColIdx)


  implicit class MapHelper(map: Map) {
    def noOfRows = map.length
    def noOfCols = map.headOption.map(_.length).getOrElse(0)

    def edgePositions = new MapEdgeIterator(map)

    def isValidPosition(position: Position): Boolean = {
      val (rowIdx, colIdx) = position

      rowIdx >= 0 && rowIdx <= noOfRows - 1 &&
        colIdx >= 0 && colIdx <= noOfCols - 1
    }

    def apply(position: Position): Elev = {
      val (rowIdx, colIdx) = position
      map(rowIdx)(colIdx)
    }

  }

  implicit class MemoHelper(memo: Memo) {
    def apply(position: Position): HighestElevInfo = {
      val (rowIdx, colIdx) = position
      memo(rowIdx)(colIdx)
    }

    def update(position: Position, highestElevInfo: HighestElevInfo) : scala.Unit = {
      val (rowIdx, colIdx) = position
      memo(rowIdx)(colIdx) = highestElevInfo
    }

    def isDefined(position: Position): Boolean = Option(memo(position)).isDefined
  }

  implicit class PositionHelper(position: Position) {

    def up: Position = position._1 -> (position._2 - 1)
    def down: Position = position._1 -> (position._2 + 1)
    def left: Position = (position._1) - 1 -> position._2
    def right: Position = (position._1) + 1 -> position._2

  }

}
