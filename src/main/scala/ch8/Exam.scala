package ch8

object Exam {
  def main(args: Array[String]): Unit = {
    val smallInt = MyGenX.choose(-10, 10)
    val maxProp = MyGenX.forAll(MyGenX.listOfN(5, smallInt)) {
      ns =>
        println(ns)
        val max = ns.max
        ns.forall(_ <= max)
    }
    val rng = ch6.SimpleRNG(System.currentTimeMillis())
    val result = maxProp.run(0, 10, rng)
    println(result)
  }
}