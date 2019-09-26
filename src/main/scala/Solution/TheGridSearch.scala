object TheGridSearch {

  def gridSearch(G: Array[String], P: Array[String]): String = {

    def getDimensions(s: Array[String]): Array[Int] = {
      val dim0 = s.size
      val dim1 = dim0 match {
        case 0 => 1
        case _ => s(0).size
      }

      Array(dim0, dim1)
    }

    val dimG = getDimensions(G)
    val dimP = getDimensions(P)

    def checkPosition(i: Int, j: Int): Boolean = {
      if (G(i)(j) == P(0)(0) &&
        i + dimP(0) <= dimG(0) &&
        j + dimP(1) <= dimG(1)) {

        for (x <- (i to i + dimP(0) - 1))
          for (y <- (j to j + dimP(1) - 1)) {
            //println(s"G(${x})(${y}) = ${G(x)(y)}, P(${x-i})(${y-j}) = ${P(x - i)(y - j)}")
            if (G(x)(y) != P(x - i)(y - j))
              return false
          }

        return true
      }

      false
    }

    for (i <- (0 to dimG(0) - dimP(0)))
      for (j <- (0 to dimG(1) - dimP(1)))
        if (checkPosition(i, j))
          return "YES"

    "NO"
  }

  def run(): Unit = {
    val G = Array("1234567890",
      "0987654321",
      "1111111111",
      "1111111111",
      "2222222222")

    val P = Array("876543",
      "111111",
      "111111")

    println(gridSearch(G, P))
  }
}

