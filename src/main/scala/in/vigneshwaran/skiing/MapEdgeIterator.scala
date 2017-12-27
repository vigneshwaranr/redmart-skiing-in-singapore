package in.vigneshwaran.skiing

class MapEdgeIterator(map: Map) extends Iterator[Position] {
  require(map.noOfRows >= 2 && map.noOfCols >= 2)

  override def hasNext: Boolean = mapEdgeIterator.hasNext

  override def next(): Position = mapEdgeIterator.next()

  private val mapEdgeIterator =
    //top edge
    colIterator(fixedRowIdx = 0) ++
    //right edge excluding the top right end
    rowIterator(fixedColIdx = map.noOfCols - 1, startingRowIdx = 1, endingRowIdx = map.noOfRows - 1) ++
    //bottom edge excluding the bottom right end
    colIterator(fixedRowIdx = map.noOfRows - 1, startingColIdx = 0, endingColIdx = map.noOfCols - 2) ++
    //left edge excluding the top left and bottom left ends
    rowIterator(fixedColIdx = 0, startingRowIdx = 1, endingRowIdx = map.noOfRows - 2)



  private def rowIterator(fixedColIdx: ColIdx, startingRowIdx: RowIdx = 0, endingRowIdx: RowIdx = map.noOfRows - 1): Iterator[Position] =
    (startingRowIdx to endingRowIdx).toIterator.map { rowIdx =>
      rowIdx -> fixedColIdx
    }

  private def colIterator(fixedRowIdx: RowIdx, startingColIdx: ColIdx = 0, endingColIdx: ColIdx = map.noOfCols - 1): Iterator[Position] =
    (startingColIdx to endingColIdx).toIterator.map { colIdx =>
      fixedRowIdx -> colIdx
    }
}
