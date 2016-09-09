package ch9

import fastparse.all._

object ExamPrefix {
  val logged = scala.collection.mutable.Buffer.empty[String]
  implicit val logger = fastparse.Logger(logged.append(_))

  def eval(tree: (String, Seq[Int])) = {
    println(tree)
    tree match {

      case ("+", nums) => nums.reduceLeft(_ + _)
      case ("-", nums) => nums.reduceLeft(_ - _)
      case ("*", nums) => nums.reduceLeft(_ * _)
      case ("/", nums) => nums.reduceLeft(_ / _)
    }
  }

  val number: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt)) //数字
  val numbers: P[Seq[Int]] = P(" ".rep ~ number ~ " ".rep ).rep

  val simpleParens: P[(String ,Seq[Int])]  = P("(".log() ~ CharIn("+-*/").!.log() ~ " ".rep ~ numbers.log() ~ " ".rep ~ ")").log()

  def main(args: Array[String]) {
    println(s"number(1)= ${number.parse("1")}")
    println(s"number(12)= ${number.parse("12")}")
    println(s"number( 12 )= ${number.parse(" 12 ")}")
    println(s"numbers( 12 )= ${numbers.parse(" 12 ")}")
    //println(s"number(1 2)= ${number.parse("1 2")}")

    println(s"number(1 2 ))= ${number.parse("1 2 )")}")
    println(s"numbers(1 2 ))= ${numbers.parse("1 2 )")}")
    println(simpleParens.parse("(+ 1     3 4 2 )"))
    println(logged.mkString("\n"))
  }

}