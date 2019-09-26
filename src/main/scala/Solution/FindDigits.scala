object FindDigits {

  def findDigits(n: Int): Int = {
    var sum = 0;
    for (ch <- n.toString()) {
      val d = ch.asDigit
      if (d != 0) {
        if (n % d == 0)
          sum += 1
      }
    }

    sum
  }

  def run(): Unit = {
    val n:Int = 1012

    println(findDigits(n))
  }
}
