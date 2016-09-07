package ch8

import ch6.State
import ch6.RNG
import ch6.SimpleRNG
import scala.collection.mutable.Buffer
import Prop._

sealed trait Result {
  def isFalsified: Boolean
}
case object Proved extends Result {
  def isFalsified = false
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
  def isFalsified = true

}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    def run(max: MaxSize, a: TestCases, rng: RNG) = {
      val r = this.run(max, a, rng)
      if (r.isFalsified) {
        r
      } else {
        p.run(max, a, rng)
      }
    }
    Prop(run)
  }

  def ||(p: Prop): Prop = {
    def run(max: MaxSize, a: TestCases, rng: RNG) = {
      val r = this.run(max, a, rng)
      if (!r.isFalsified) {
        r
      } else {
        p.run(max, a, rng)
      }
    }
    Prop(run)
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Passed else Falsified("()", 0)
  }

  def run(p: Prop, maxSize: Int = 90,
          testCases: Int = 100,
          rng: RNG = ch6.SimpleRNG(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }
}

case class SGen[+A](forSize: Int => MyGen[A])

case class MyGen[+A](sample: State[RNG, A]) {

  import MyGenX._

  def flatMap[B](f: A => MyGen[B]): MyGen[B] = {
    val run = (rng: RNG) => {
      val (a, next) = sample.run(rng)
      val (b, next2) = f(a).sample.run(next)
      (b, next2)
    }
    MyGen(State(run))
  }

  def map[B](f: A => B): MyGen[B] = {
    def ff(a: A): MyGen[B] = {
      unit(f(a))
    }
    flatMap(ff)
  }

  def foreach[B](f: A => B): Unit = {
    map(f)
  }

  def listOfN(size: MyGen[Int]): MyGen[List[A]] = {
    def ff(i: Int): MyGen[List[A]] = {
      MyGenX.listOfN(i, this)
    }
    size.flatMap { ff }
  }

  def unsized: SGen[A] = SGen { a: Int => this }
}

object MyGenX {

  def choose(start: Int, stop: Int): MyGen[Int] = {
    def f(i: Int): Int = {
      val d = stop - start
      if (d == 0) {
        start
      } else {
        start + (i.abs % d)
      }
    }
    val run = (rng: RNG) => {
      val (i, nextRng) = rng.nextInt
      (f(i), nextRng)
    }

    MyGen(State(run))
  }

  def double: MyGen[Double] = {
    val run = (rng: RNG) => {
      ch6.RNG.double(rng)
    }
    MyGen(State(run))
  }

  def atoz(): MyGen[Char] = {
    choose('a'.toInt, 'z'.toInt).map(_.toChar)
  }

  val atoZ: MyGen[Char] = {
    val g1 = choose('a'.toInt, 'z'.toInt).map(_.toChar)
    val g2 = choose('A'.toInt, 'Z'.toInt).map(_.toChar)
    union(g1, g2)
  }

  val utf8: MyGen[Char] = {
    val g1 = choose('a'.toInt, 'z'.toInt).map(_.toChar)
    val g2 = choose('A'.toInt, 'Z'.toInt).map(_.toChar)
    val g3 = choose('啊'.toInt, '中'.toInt).map(_.toChar)
    union(g3, union(g1, g2))
  }

  def string(n: Int = 10, gen: MyGen[Char] = utf8): MyGen[String] = {
    choose(0, n).flatMap {
      i => listOfN(i, gen).map { _.mkString }
    }
  }

  def unit[A](a: => A): MyGen[A] = {
    val run = (rng: RNG) => {
      val (i, nextRng) = rng.nextInt
      (a, nextRng)
    }
    MyGen(State(run))
  }
  def boolean: MyGen[Boolean] = {
    val run = (rng: RNG) => {
      val (i, nextRng) = rng.nextInt
      val r = if (i > 0) true else false
      (r, nextRng)
    }
    MyGen(State(run))
  }

  def union[A](g1: MyGen[A], g2: MyGen[A]): MyGen[A] = {
    boolean.flatMap { x => if (x) g1 else g2 }
  }

  def weighted[A](g1: (MyGen[A], Double), g2: (MyGen[A], Double)): MyGen[A] = {
    val total = g1._2 + g2._2
    if (total <= 0 || g1._2 <= 0 || g1._2 <= 0) {
      union(g1._1, g2._1)
    } else {
      val d1 = g1._2 / total
      choose(0, 1000).flatMap { i =>
        val d = i.toDouble / 1000
        if (d <= d1) g1._1 else g2._1
      }
    }
  }

  def listOfN[A](n: Int, g: MyGen[A]): MyGen[List[A]] = {
    val run = (rng: RNG) => {
      val buf: Buffer[A] = Buffer[A]()
      var next = rng
      for (i <- 0 until n) {
        val (a, next1) = g.sample.run(next)
        next = next1
        buf.append(a)
      }
      (buf.toList, next)
    }
    MyGen(State(run))
  }

  def listOf[A](g: MyGen[A]): SGen[List[A]] = {
    def ff(i: Int) = listOfN(i, g)
    SGen(ff)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
    forAll(g.forSize)(f)
  }

  def forAll[A](g: Int => MyGen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: ch5.Stream[Prop] = ch5.Stream.from(0).take(n min max).map {
        i => forAll(g(i))(f)
      }
      val prop: Prop = props.map { p =>
        Prop { (max1, n1, rng1) =>
          p.run(max1, casesPerSize, rng1)
        }

      }.toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: MyGen[A])(f: A => Boolean): Prop = {

    /**
     * 随机 A 流
     *
     */
    def randomStream[A](g: MyGen[A])(rng: RNG): ch5.Stream[A] = {
      ch5.Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    }

    def msg[A](a: A, e: Exception): FailedCase = {
      s"test case $a \n Exception ${e.getMessage}"
    }

    def run(max: MaxSize, n: TestCases, rng: RNG): Result = {
      val step1: ch5.Stream[Result] =
        randomStream(as)(rng).zip(ch5.Stream.from(0)).take(n).map {
          case (a, i) => try {
            if (f(a))
              Passed
            else
              Falsified(a.toString, i)

          } catch {
            case e: Exception => Falsified(msg(a, e), i)
          }
        }
      step1.filter { x => x.isFalsified }.headOption.getOrElse(Passed)
    }
    Prop(run)
  }
}

