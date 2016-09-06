package ch8

import ch6.State
import ch6.RNG
import ch6.SimpleRNG

trait MyGen[A] {

}

trait Prop{
  
}
case class MyGen[A](sample: State[RNG,A]){
  
  val rng: RNG = SimpleRNG(0L)
  def choose(s: Int, e: Int): MyGen[Int] = {
    val f = sample.run
    val(a,nextRng) = f(rng)
    val s = State( sample.run )
    MyGen(s)
  }
}

object MyGen {
  
  def choose[A](a: A, b: A): MyGen[A] = {
    val f =  { i: Int => a}
    
  }
  
  
  def listOfN[A](n: Int, a: MyGen[A]): MyGen[List[A]] = {

  }
  
  def forAll[A](a: MyGen[A])(f: A=> Boolean): Prop ={
    
  }
}
