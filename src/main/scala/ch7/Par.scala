package ch7

import java.util.concurrent.TimeUnit
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.Callable
import java.util.concurrent.Executors

//class ExecutorService  {
//  def submit[A](a: Callable[A]): Future[A]
//}
//trait Callable[A] { def call: A }
//
//trait Future[A] {
//  def get: A
//  def get(timeout: Long, unit: TimeUnit): A
//  def cancel(evenIfRunning: Boolean): Boolean
//  def isDone: Boolean
//  def isCancelled: Boolean
//}

/**
map  = (a:Par[A])(f: A => B): Par[B]
 * 
 * map(y)(id) = y 
 * f= A => B
 * y = unit(f)
 * map(unit(f))(id) = unit((f(x))
id(f) = f(id) 
map(unit(f))(id) = map(unit(id))(f) 
map(unit(id))(f)  = map(unit(x))(f)

map(unit(x))(f) = unit((f(x))

 * 
 *  
 * 
 * 
 * 
 */
object Par {
  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](get: A) extends Future[A] {
    def get(timeout: Long, unit: TimeUnit): A = get
    def cancel(evenIfRunning: Boolean): Boolean = false
    def isDone: Boolean = true
    def isCancelled: Boolean = false
  }

  def unit[A](a: => A): Par[A] = { s => UnitFuture(a)

  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
    a(s)
  }

  def map2[A, B, C](l: Par[A], r: Par[B])(f: (A, B) => C): Par[C] = { es =>
    val af = l(es)
    val bf = r(es)
    val c = f(af.get, bf.get)
    UnitFuture(c)

  }

  def fork[A](a: => Par[A]): Par[A] = { es =>
    es.submit(new Callable[A] {
      def call = a(es).get
    })
  }

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => unit(f(a))
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit(()))((a, _) => f(a))

  }
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps match {
      case Nil => unit(Nil)
      case a :: t =>
        val tmp: Par[List[A]] = sequence(t)
        map2(tmp, a) { (ps2, x) => x :: ps2 }
    }
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val f2 = (x: A) => (x, f(x))
    val tmp = parMap(as)(f2)
    map(tmp) { as => as.collect { case (a1, true) => a1 } }

  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
}
object Exam {

  def main(args: Array[String]): Unit = {
    val s = sum((1 to 50))
    val es = Executors.newFixedThreadPool(10)
    val r1 = s.apply(es)
    val r2 = r1.get
    println(s"r2=${r2}")
  }

  def sum(ints: IndexedSeq[Int]): Par.Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }
}