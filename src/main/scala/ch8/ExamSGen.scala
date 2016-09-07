package ch8

object ExamSGen {

  def main(args: Array[String]): Unit = {
    val smallInt = MyGenX.choose(-10, 10)
    val sGen = MyGenX.listOf(smallInt)
    val maxProp = MyGenX.forAll(sGen) { ns =>
      println(ns)
      if (ns.isEmpty) {
        true
      } else {
        val max = ns.max
        ns.forall(_ <= max)
      }
    }

    val rng = ch6.SimpleRNG(System.currentTimeMillis())
    val result = maxProp.run(10, 10, rng)
    println(result)
  }
}