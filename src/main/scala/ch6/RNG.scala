package ch6

trait RNG {
  def nextInt: (Int, RNG)

}
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}
object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    ((n1, n2), rng2)
  }

  def noNegativeInt(rng: RNG): (Int, RNG) = {
    val (n1, rng1) = rng.nextInt
    if (n1 >= 0) {
      (n1, rng1)
    } else {
      (Math.abs(n1 + 1), rng1)
    }
  }

  def int(rng: RNG): (Int, RNG) = rng.nextInt
  
  def double(rng: RNG): (Double, RNG) = {
    val (n1, rng1) = rng.nextInt
    val y = Integer.MAX_VALUE.toDouble + 1.0
    val x = n1.toDouble / y
    (Math.abs(x), rng1)
  }
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (d1, rng2) = double(rng1)
    ((n1, d1), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n1, d1), rng2) = intDouble(rng)
    ((d1, n1), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }
  
  @annotation.tailrec
  def many[A](count: Int, res: List[A])(f: RNG => (A,RNG))(r: RNG): (List[A], RNG) = {
      if (count <= 0) {
        (res, r)
      } else {
        val (n, r1) = f(r)
        many(count - 1, n :: res)(f)(r1)
      }
    }
 
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    many(count, Nil)( RNG.int)(rng)
  }
}

object Exam {
  import RNG._
  def main(args: Array[String]): Unit = {
    val rng = new SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    println(s"n1=${n1},\t rng2=${rng2}")
    val (n2, rng3) = rng2.nextInt
    println(s"n2=${n2},\t rng3=${rng3}")
    val ((x1, x2), rng4) = randomPair(rng3)
    println(s"(x1,x2)=${(x1, x2)},\t rng4=${rng4}")
    println(s"double=${double(rng4)}")
    println(s"ints(3)=${ints(3)(rng)}")
  }
}