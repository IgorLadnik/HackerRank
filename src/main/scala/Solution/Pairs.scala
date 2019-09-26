object Pairs {

  // Complete the pairs function below.
  def pairs(k: Int, arr: Array[Int]): Int = {

    val dctMain = scala.collection.mutable.Map[Int, Int]()
    val dctDuplications = scala.collection.mutable.Map[Int, Int]()
    for (key <- arr) {
      if (!dctMain.contains(key))
        dctMain(key) = key - k
      else {
        if (!dctDuplications.contains(key))
          dctDuplications(key) = 2
        else
          dctDuplications(key) = dctDuplications(key) + 1
      }
    }

    var count = 0
    for (key0 <- dctMain.keys) {
      val key1 = dctMain(key0)
      if (dctMain.contains(key1)) {
        if (dctDuplications.contains(key0) && dctDuplications.contains(key1))
          count += dctDuplications(key0) * dctDuplications(key1)
        else
          count += 1
      }
    }

    count
  }

  def run() {

    val k = 2
    val arr = Array(3, 1, 5, 4, 5, 2, 5)

    println(pairs(k, arr))
  }
}
