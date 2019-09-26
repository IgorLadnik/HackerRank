object PowerEX {

  def powerE(x: Double, maxTerm: Int = 10): Double = {
    var result = 1 + x
    for (i <- 2 to maxTerm - 1) {
      var m = x
      var d = 1
      for (j <- 2 to i) {
        m *= x
        d *= j
      }

      result += m / d
    }

    result - (result % 0.0001)
  }


  def run() {
    var result = powerE(20)
    println(result)
  }
}
