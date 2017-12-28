package in.vigneshwaran.skiing

class SkiRouteFinder {

  def findLongestAndSteepestDive(map: Map): (Length, Drop) = {
    require(map.noOfRows > 0 && map.noOfCols > 0)
    
    val memo = Array.ofDim[HighestElevInfo](map.noOfRows, map.noOfCols)

    val elevInfo = for {
      rowIdx <- 0 until map.noOfRows
      colIdx <- 0 until map.noOfCols
    } yield {
      //print(s"\nStarting at $rowIdx, $colIdx -> ${map(rowIdx)(colIdx)}: ")
      val endElev = map(rowIdx)(colIdx)
      val (lengthToHighestElev, highestElev) =
        findLengthToHighestElev(map, position = rowIdx -> colIdx, memo)

      val drop = highestElev - endElev
      lengthToHighestElev -> drop
    }

    val (longestDrop, steepestDrop) = getHighestElevInfo(elevInfo.toList) //elevations mapped to drop size
    longestDrop -> steepestDrop
  }

  private def findLengthToHighestElev(map: Map, position: Position, memo: Memo): HighestElevInfo = {
    //print(s"${map(position)} -> ")
    if (memo.isDefined(position)) memo(position)
    else {
      val currentElev = map(position)

      val neighboringPositions = List(position.up, position.right, position.down, position.left)

      val elevationsFromHere: List[HighestElevInfo] = neighboringPositions.map { nextPosition =>
        if (!map.isValidPosition(nextPosition)) {
          0 -> currentElev
        } else if (map(nextPosition) > currentElev) {
          val (lengthToHighestElev, highestElev) = findLengthToHighestElev(map, nextPosition, memo)
          (lengthToHighestElev + 1) -> highestElev
        } else {
          1 -> currentElev
        }
      }

      val highestElevInfo = getHighestElevInfo(elevationsFromHere)
      memo.update(position, highestElevInfo)
      highestElevInfo
    }
  }

  private def getHighestElevInfo(elevationInfoList: List[HighestElevInfo]): HighestElevInfo = {
    elevationInfoList.foldLeft((0, 0)) { case (maxElevInfo, elevInfo) =>
      val (currentLengthToElev, currentElev) = elevInfo
      val (maxLengthToElev, maxElev) = maxElevInfo

      if (currentLengthToElev > maxLengthToElev) {
        currentLengthToElev -> currentElev
      } else if (currentLengthToElev == maxLengthToElev && currentElev > maxElev) {
        currentLengthToElev -> currentElev
      } else {
        maxLengthToElev -> maxElev
      }
    }
  }

}
