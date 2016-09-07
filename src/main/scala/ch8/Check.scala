package ch8

import ch6.State
import ch6.RNG
import ch6.SimpleRNG
import scala.collection.mutable.Buffer
import Prop._

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
  def isFalsified = true

}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    def run(a: TestCases, rng: RNG) = {
      val r = this.run(a, rng)
      if (r.isFalsified) {
        r
      } else {
        p.run(a, rng)
      }
    }
    Prop(run)
  }

  def ||(p: Prop): Prop = {
    def run(a: TestCases, rng: RNG) = {
      val r = this.run(a, rng)
      if (!r.isFalsified) {
        r
      } else {
        p.run(a, rng)
      }
    }
    Prop(run)
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
}

case class MyGen[A](sample: State[RNG, A]) {

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

  def listOfN(size: MyGen[Int]): MyGen[List[A]] = {
    def ff(i: Int): MyGen[List[A]] = {
      MyGenX.listOfN(i, this)
    }
    size.flatMap { ff }
  }

}

object MyGenX {

  def choose(start: Int, stop: Int): MyGen[Int] = {
    def f(i: Int): Int = {
      val d = stop - start
      if (d == 0) {
        start
      } else {
        start + i % d
      }
    }
    val run = (rng: RNG) => {
      val (i, nextRng) = rng.nextInt
      (f(i), nextRng)
    }

    MyGen(State(run))
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
      for (i <- 0 to n) {
        val (a, next1) = g.sample.run(next)
        next = next1
        buf.append(a)
      }
      (buf.toList, next)
    }
    MyGen(State(run))
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

    def run(n: TestCases, rng: RNG): Result = {
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
