object AlternatingCharacters {

  def alternatingCharacters(s: String): Int = {
    var prevCh = '_'
    var sum = 0
    for (ch <- s) {
      if (ch != 'A' && ch != 'B')
        return -1

      if (prevCh == ch)
        sum += 1
      else
        prevCh = ch
    }

    sum
  }

  def run() {

    //val s = "AAAA"
    //val s = "BBBBB"
    //val s = "ABABABAB"
    //val s = "BABABA"
    val s = "AAABBB"

    val result = alternatingCharacters(s)

    println(result)
  }
}