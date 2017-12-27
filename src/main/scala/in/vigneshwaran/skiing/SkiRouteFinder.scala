package in.vigneshwaran.skiing

class SkiRouteFinder {

  def findLongestAndDeepRoute(map: Map): (Length, Drop) = {
    require(map.noOfRows > 0 && map.noOfCols > 0)
    
    val memo = Array.ofDim[HighestElevInfo](map.noOfRows, map.noOfCols)

    val elevInfo = map.edgePositions.map { case (rowIdx, colIdx) =>
      val endElev = map(rowIdx)(colIdx)
      val (lengthToHighestElev, highestElev) =
        findLengthToHighestElev(map, startingPosition = rowIdx -> colIdx, memo)

      val drop = highestElev - endElev
      lengthToHighestElev -> drop
    }

    val (longestDrop, steepestDrop) = getHighestElevInfo(elevInfo.toList) //elevations mapped to drop size
    longestDrop -> steepestDrop
  }

  private def findLengthToHighestElev(map: Map, startingPosition: Position, memo: Memo): HighestElevInfo = {
    findLengthToHighestElevIter(map, startingPosition, previousElev = -1, memo)
  }

  def findLengthToHighestElevIter(map: Map, position: Position, previousElev: Elev, memo: Memo): HighestElevInfo = {
    if (memo.isDefined(position)) memo(position)
    else {
      val currentElev = map(position)

      val neighboringPositions = List(position.up, position.right, position.down, position.left)

      val elevationsFromHere: List[HighestElevInfo] = neighboringPositions.map { nextPosition =>
        if (map.isValidPosition(nextPosition) && map(nextPosition) > currentElev) {
          val (lengthToHighestElev, highestElev) = findLengthToHighestElevIter(map, nextPosition, currentElev, memo)
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
