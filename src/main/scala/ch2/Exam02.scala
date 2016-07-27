package ch2

object Exam02 {
  @annotation.tailrec
  def isSorted[A](as: List[A])(ordered: (A, A) => Boolean): Boolean = {
    as match {
      case Nil          => true
      case List(x)      => true
      case List(x1, x2) => ordered(x1, x2)
      case x1 :: tail   => isSorted(tail)(ordered)
    }
  }

  @annotation.tailrec
  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    as match {
      case Array()       => true
      case Array(x)      => true
      case Array(x1, x2) => ordered(x1, x2)
      case _             => isSorted(as.drop(1))(ordered)
    }
  }

  def main(args: Array[String]): Unit = {
    def o(x: Int, y: Int) = x < y
    val r1 = isSorted((1 to 10).toList)(o)
    println(r1)
    val r2 = isSorted((10.to(1, -1)).toList)(o)
    println(r2)
    println(isSorted(Nil)(o))
    println(isSorted(Array(1, 2, 3, 10, 9))(o))
    println(isSorted(Array(1, 2, 3, 9, 9))(o))
    println(isSorted(Array(1, 2, 3, 9, 10))(o))
  }
}