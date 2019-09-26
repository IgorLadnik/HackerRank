object ConnectedCellsInGrid {
  import scala.collection.mutable.ArrayBuffer

  def connectedCell(matrix: Array[Array[Int]]): Int = {

    class Cell(v: Int) {
      def isFilled: Boolean = { v == 1 }
      var group = -1
    }

    var n: Int = 0
    var m: Int = 0
    var cells: Array[Array[Cell]] = Array.ofDim[Cell](n, m)
    var maxGroup = -1
    var arrSum = ArrayBuffer[Int]()

    def initCells(matrix: Array[Array[Int]]) = {
      n = matrix.length
      m = matrix(0).length
      cells = Array.ofDim[Cell](n, m)
      for (i <- (0 to n -1))
        for (j <- (0 to m -1))
          cells(i)(j) = new Cell(matrix(i)(j))
    }

    def mergeGroups(p: Int, q: Int) = {
      var minGroupNum = p
      var maxGroupNum = q
      if (q < minGroupNum) {
        minGroupNum = q
        maxGroupNum = p
      }

      for (i <- (0 to n -1))
        for (j <- (0 to m -1))
          if (cells(i)(j).group == maxGroupNum)
            cells(i)(j).group == minGroupNum

      arrSum(minGroupNum) += arrSum(maxGroupNum)
      arrSum(maxGroupNum) = 0
    }

    def SetGrouptoMaxGroup(i: Int, j: Int) = {
      maxGroup += 1
      cells(i)(j).group = maxGroup
      arrSum += 1
      arrSum(maxGroup) = 1
    }

    def checkGroup(i: Int, j: Int): Unit = {
      if (i < 0 || i >= n || j < 0 || j >= n)
        return

      if (!cells(i)(j).isFilled)
        return

      if (maxGroup == -1)
        SetGrouptoMaxGroup(i, j)

      for (x <- (i - 1 to i + 1)) {
        if (x >= 0 && x < n) {
          for (y <- (j - 1 to j + 1)) {
            if (y >= 0 && y < m && !(x == i && y == j)) {
              if (cells(x)(y).isFilled) {

                val groupThis = cells(i)(j).group
                val groupAdj = cells(x)(y).group

                if (groupThis == -1 && groupAdj > -1) {
                  // cell(x, y) belongs to some group, and cell(i, j) does not
                  cells(i)(j).group = groupAdj
                  arrSum(groupAdj) += 1
                }
                else {
                  if (groupThis > -1 && groupAdj == -1) {
                    // cell(i, j) belongs to some group, and cell(x, y) does not
                    cells(x)(y).group = groupThis
                    arrSum(groupThis) += 1
                  }
                  else
                  if (groupThis != groupAdj)
                  // The cells belong to different groups - groups merge is required
                    mergeGroups(groupThis, groupAdj)
                }

                if (groupThis == -1 && groupAdj == -1)
                  SetGrouptoMaxGroup(i, j)
              }
            }
          }
        }
      }
    }

    // Actual actions
    initCells(matrix)
    for (i <- (0 to n -  1))
      for (j <- (0 to m - 1))
        checkGroup(i, j)

    if (arrSum.size > 0)
      arrSum.max
    else
      0
  }

  def run(): Unit = {

    /*
    val matrix: Array[Array[Int]] =
                  Array(
                    Array(1, 1, 0, 1),
                    Array(0, 1, 1, 0),
                    Array(0, 0, 1, 0),
                    Array(1, 0, 0, 0))
    */
    /*
    val matrix: Array[Array[Int]] =
      Array(
        Array(0, 0, 0, 0),
        Array(0, 0, 0, 0),
        Array(0, 0, 0, 0),
        Array(0, 0, 0, 0))
    */

    val matrix: Array[Array[Int]] =
      Array(
        Array(0, 1, 0, 0, 0, 0, 1, 1, 0),
        Array(1, 1, 0, 0, 1, 0, 0, 0, 1),
        Array(0, 0, 0, 0, 1, 0, 1, 0, 0),
        Array(0, 1, 1, 1, 0, 1, 0, 1, 1),
        Array(0, 1, 1, 1, 0, 0, 1, 1, 0),
        Array(0, 1, 0, 1, 1, 0, 1, 1, 0),
        Array(0, 1, 0, 0, 1, 1, 0, 1, 1),
        Array(1, 0, 1, 1, 1, 1, 0, 0, 0))

    println(connectedCell(matrix))
  }
}

