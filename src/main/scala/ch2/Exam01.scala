package ch2

object Exam01 {
  def fib(n: Int): Int = n match {
    case 0            => 0
    case 1            => 1
    case x if (x < 0) => 0
    case x            => fib(x - 1) + fib(x - 2)
  }

  def fib2(n1: Int): Int = {
    @annotation.tailrec
    def f(n: Int, s1: Int, s2: Int): Int = n match {
      case 0            => s1
      case 1            => s2
      case x if (x < 0) => 0
      case x            => f(x - 1, s2, s2 + s1)
    }
    f(n1, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    List(fib _, fib2 _).map { (-2 to 10).map(_).mkString(",") }.
      foreach(println)

  }
}