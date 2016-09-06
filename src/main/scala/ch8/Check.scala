package ch8

import ch6.State
import ch6.RNG
import ch6.SimpleRNG



trait Prop {

}
case class MyGen[A](sample: State[RNG, A]) {

  
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
  
  def unit[A](a: => A): MyGen[A] ={
    val run = (rng: RNG) => {
      val (i, nextRng) = rng.nextInt
      (a , nextRng)
    }
     MyGen(State(run))
  }
}

object MyGenX {

  def choose[A](a: A, b: A): MyGen[A] = {
    val f = { i: Int => a }

  }

  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = {

  }

  def forAll[A](a: MyGen[A])(f: A => Boolean): Prop = {

  }
}
