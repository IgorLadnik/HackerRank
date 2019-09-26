object MissingNumbers {
  import scala.collection.mutable.ArrayBuffer

  // Complete the missingNumbers function below.
  def missingNumbers(b: Array[Int], a: Array[Int]): Array[Int] = {

    if (b.length < 1 || b.length > 2e5 ||  a.length < b.length)
      return Array[Int]()

    scala.util.Sorting.quickSort(a)
    scala.util.Sorting.quickSort(b)

    if (a(a.length - 1) - a(0) > 100)
      return Array[Int]()

    val missing = new ArrayBuffer[Int]()

    var missingNum = 0
    var indexB = 0
    for (i <- 0 to a.length - 1) {
      indexB = i - missingNum
      if (indexB >= b.length || a(i) != b(indexB)) {
        if (!missing.contains(a(i)))
          missing += a(i)

        missingNum += 1
      }
    }

    missing.to[Array]
  }

  def run() {

    val arr = Array(203, 204,      205, 206, 207,      208, 203, 204, 205, 206)
    val brr = Array(203, 204, 204, 205, 206, 207, 205, 208, 203, 206, 205, 206, 204)

    //val arr = Array(11, 4, 11, 7,           13, 4,    12, 11, 10, 14)
    //val brr = Array(11, 4, 11, 7, 3, 7, 10, 13, 4, 8, 12, 11, 10, 14, 12)

    //val arr = Array(12)
    //val brr = Array(11, 4, 11, 7, 3, 7, 10, 13, 4, 8, 12, 11, 10, 14, 12)

    var missing = missingNumbers(arr, brr)

    print("Output: ")
    for (n <- missing)
      print(s"${n} ")

    println()
  }
}
