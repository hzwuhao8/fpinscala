package ch2

object Exam02 {
  def isSorted[A](as: List[A])(ordered: (A, A) => Boolean): Boolean = {
    as match {
      case Nil          => true
      case List(x)      => true
      case List(x1, x2) => ordered(x1, x2)
      case x1 :: tail   => isSorted(tail)(ordered)
    }
  }

  def main(args: Array[String]): Unit = {
    def o(x: Int, y: Int) = x < y
    val r1 = isSorted((1 to 10).toList)(o)
    println(r1)
    val r2 = isSorted((10.to(1,-1)).toList)(o)
    println(r2)
    println(isSorted(Nil)(o))
  }
}