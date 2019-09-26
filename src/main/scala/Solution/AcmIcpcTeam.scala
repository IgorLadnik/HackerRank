object AcmIcpcTeam {
  import scala.collection.mutable.ArrayBuffer

  def acmTeam(topic: Array[String]): Array[Int] = {

    var n = topic.length
    var m = topic(0).length

    if (n < 2 || n > 500 || m < 1 || m > 500)
      return Array(0, 0)

    var arrTopic = Array.ofDim[Int](n, m)

    def stringToIntegers(s: String): Array[Int] = {
      var buf = ArrayBuffer[Int]()
      for (i <- 0 to m - 1) {
        var n = 0
        if (s(i) == '1')
          n = 1

        buf += n
      }

      buf.to[Array]
    }

    def getCommandSkills(i: Int, j: Int) : Int = {
      var sum = 0
      for (k <- 0 to m - 1)
        if (arrTopic(i)(k) + arrTopic(j)(k) > 0)
          sum += 1

      sum
    }

    for (i <- 0 to n - 1)
      arrTopic(i) = stringToIntegers(topic(i))

    var maxSkills = 0
    var commandsNum = 0
    for (i <- 0 to n - 1)
      for (j <- i + 1 to n - 1) {
        val skills = getCommandSkills(i, j)
        if (skills == maxSkills)
          commandsNum += 1
        else if (skills > maxSkills) {
          maxSkills = skills
          commandsNum = 1
        }
      }

    Array(maxSkills, commandsNum)
  }

  def run() {

    var topic = Array("10101",
      "11100",
      "11010",
      "00101")

    val result = acmTeam(topic)

    println(s"${result(0)}  ${result(1)}")
  }
}
