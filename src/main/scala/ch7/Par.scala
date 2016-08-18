package ch7

import java.util.concurrent.TimeUnit

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
trait Callable[A] { def call: A }

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

 

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
}
object Exam {

  def main(args: Array[String]): Unit = {

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