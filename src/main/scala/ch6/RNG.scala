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
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = (r: RNG) => (a, r)

  def int: Rand[Int] = (r: RNG) => r.nextInt

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
      val (a, r2) = s.apply(rng)
      (f(a), r2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, r1) = ra.apply(rng)
    val (b, r2) = rb.apply(r1)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((a, b) => (a, b))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs match {
      case Nil => rng => (Nil, rng)
      case a :: t => rng =>
        val (n1, r1) = a.apply(rng)
        val rr = sequence(t)(r1)
        val (n3, r3) = rr
        (n1 :: n3, r3)
    }
  }

  def sequenceFoldleft[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val z = (rng: RNG) => (List.empty[A], rng)
    val f = (b: Rand[List[A]], a: Rand[A]) => { (rng: RNG) =>
      val (l, r2) = b(rng)
      val (a1, r3) = a(r2)
      (l ::: List(a1), r3)
    }
    fs.foldLeft(z)(f)

  }
  def sequenceFoldright[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val z = unit(List.empty[A])
    val f = (a: Rand[A], b: Rand[List[A]]) => { (rng: RNG) =>
      val (l, r2) = b(rng)
      val (a1, r3) = a(r2)
      (a1 :: l, r3)
    }
    val f2 = (f: Rand[A], acc: Rand[List[A]]) => map2(f,acc)( _ :: _  )
    fs.foldRight(z)(f2)

  }
  def sequenceMap2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val z = (rng: RNG) => (List.empty[A], rng)
    fs match {
      case Nil => z
      case List(a) => rng => {
        val (n1, r2) = a(rng)
        (List(n1), r2)
      }
      case List(a, b) => map2(a, b)((a, b) => List(a, b))
      case a1 :: a2 :: t => map2(a1, a2)((a1, a2) => List(a1, a2))
    }
  }
  def intDouble2(rng: RNG): ((Int, Double), RNG) = {
    val ra = int
    val rb = double _
    val f = (a: Int, b: Double) => (a, b)
    map2(ra, rb)(f).apply(rng)
  }

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
  def many[A](count: Int, res: List[A])(f: RNG => (A, RNG))(r: RNG): (List[A], RNG) = {
    if (count <= 0) {
      (res, r)
    } else {
      val (n, r1) = f(r)
      many(count - 1, res ::: List(n))(f)(r1)
    }
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    many(count, Nil)(RNG.int)(rng)
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
    println(s"ints(4)=${ints(4)(rng)}")
    println(s"sequencs(4)=${val l1 = List.fill(4)(int); sequence(l1)(rng)}")
    println(s"sequence: ${sequence(List(unit(1), unit(2), unit(3)))(rng)}")
    println(s"sequenceFoldleft: ${sequenceFoldleft(List(unit(1), unit(2), unit(3)))(rng)}")
    println(s"sequenceFoldright: ${sequenceFoldright(List(unit(1), unit(2), unit(3)))(rng)}")
    println(s"sequenceFoldright: ${sequenceFoldright(List(int,int,int))(rng)}")
    
    println(s"intdouble=${intDouble(rng4)}")
    println(s"intdouble2=${intDouble2(rng4)}")
  }
}