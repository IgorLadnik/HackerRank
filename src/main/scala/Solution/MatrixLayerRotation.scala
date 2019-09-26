object MatrixLayerRotation {

  var n: Int = -1
  var m: Int = -1

  def rotateRing(k: Int, matrix: Array[Array[Int]], r: Int) {
    val xMax = getNumOfItemsInRing(k)
    val arr = new Array[Int](xMax)
    for (x <- 0 until xMax) {
      val tuple = x2ij(x, k)
      arr(move(x, xMax, r)) = matrix(tuple._1)(tuple._2)
    }

    for (x <- 0 until xMax) {
      val tuple = x2ij(x, k)
      matrix(tuple._1)(tuple._2) = arr(x)
    }
  }

  def x2ij(x: Int, k: Int): Tuple2[Int, Int] = {
    if (k >= getNumOfRings())
      return null

    val boundaryX = Array(
      0,
      n - 2 * k - 1,
      n + m - 4 * k - 2,
      2 * n + m - 6 * k - 3,
      2 * n + 2 * m - 8 * k - 4
    )

    // x - sequential number
    if (x >= 2 * n + 2 * m - 8 * k - 4)
      return null

    val part = getPart(k, x, boundaryX)

    part match {
      case 0 => Tuple2[Int, Int](x + k, k)
      case 1 => Tuple2[Int, Int](n - k - 1, x - (n - 2 * k) + k + 1)
      case 2 => Tuple2[Int, Int](n - k - 1 - (x - (n - 2 * k) - (m - 2 * k) + 2), m - k - 1)
      case 3 => Tuple2[Int, Int](k, m - k - 1 - (x - 2 * (n - 2 * k) - (m - 2 * k) + 3))
    }
  }

  def move(x: Int, xMax: Int, r: Int): Int = (x + r) % xMax

  def getNumOfItemsInRing(k: Int): Int = 2 * n + 2 * m - 8 * k - 4

  def getNumOfRings(): Int = {
    var z = n
    if (m < n)
      z = m
    z / 2 + z % 2;
  }

  def getPart(k: Int, x: Int, boundaryX: Array[Int]): Int = {
    for (y <- (0 until boundaryX.length)) {
      if (boundaryX(y) <= x && x < boundaryX(y + 1))
        return y
    }
    -1
  }

  def printMatrix(matrix: Array[Array[Int]]) {
    for (row <- matrix) {
      for (cell <- row)
        print(s"${cell} ")

      println()
    }
  }

  def matrixRotation(matrix: Array[Array[Int]], r: Int) {
    n = matrix.length
    if (n < 1)
      throw new Exception()

    m = matrix(0).length

    for (k <- 0 until getNumOfRings())
      rotateRing(k, matrix, r)

    printMatrix(matrix)
  }

  def run() {

    val matrix: Array[Array[Int]] =
      Array(
        Array(1, 2, 3, 4),
        Array(12, 1, 2, 5),
        Array(11, 4, 3, 6),
        Array(10, 9, 8, 7))

    matrixRotation(matrix, 2)
  }
}
