object SherlockAndArray {

  def balancedSums(a: Array[Int]): String = {
    val len = a.length
    if (len == 0)
      return "NO"

    if (len == 1)
      return "YES"

    var sumR = a.sum

    var sumL = 0
    for (i <- 1 to len - 1) {
      sumR -= a(i - 1)

      if (i > 1)
        sumL += a(i -2)

      if (sumL == sumR)
        return "YES"
    }

    "NO"
  }

  def run() {

    //var arr = Array(1, 1, 4, 1, 1)
    //var arr = Array(2, 0, 0, 0)
    //var arr = Array(0, 0, 2, 0)
    //var arr = Array(1, 2, 3)
    var arr = Array(1, 2, 3, 3)

    val result = balancedSums(arr)

    println(result)
  }
}

