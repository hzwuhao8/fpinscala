package ch10

object Exam01 {
  def main(args: Array[String]) {
    val seq = 1 to 10 
    Monoid.ordered(seq)
    Monoid.ordered( 10.to(1,-1))
    Monoid.ordered( Vector(1,5,8,2))
  }
}