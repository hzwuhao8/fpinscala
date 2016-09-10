package ch9

import fastparse.all._

/**
 * 前缀 表达式
 * 支持 整数运算的 S 表达式 
 * Lisp
 * 如何进行扩展呢
 * 定义函数？
 */
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
  val numbers: P[Seq[Int]] = P(" ".rep ~ number ~ " ".rep).rep // 这个有问题，不够严格

  val simpleParens: P[Int] = P(" ".rep ~ "(".log() ~ " ".rep ~ CharIn("+-*/").!.log() ~ " ".rep ~ numbers.log() ~ " ".rep ~ ")").map(eval).log()

  val expr0: P[(String, Seq[Int])] = P(" ".rep ~ "(".log() ~ " ".rep ~ CharIn("+-*/").!.log() ~ " ".rep ~ (" ".rep ~ number ~ " ".rep | simpleParens | expr).rep ~ " ".rep ~ ")")
  val expr: P[Int] = expr0.map(eval)

  def run(str: String): Int = {
    val r = expr.parse(str).get
    r.value
  }

  def main(args: Array[String]) {
    println(s"number(1)= ${number.parse("1")}")
    println(s"number(12)= ${number.parse("12")}")
    println(s"number( 12 )= ${number.parse(" 12 ")}")
    println(s"numbers( 12 )= ${numbers.parse(" 12 ")}")
    //println(s"number(1 2)= ${number.parse("1 2")}")

    println(s"number(1 2 ))= ${number.parse("1 2 )")}")
    println(s"numbers(1 2 ))= ${numbers.parse("1 2 )")}")
    println(s"numbers(1    ( *  3 4 )  2 ))= ${numbers.parse("1    ( *  3 4 )  2")}")
    println(simpleParens.parse("(+ 1     3 4 2 )"))
    println(simpleParens.parse(" ( +  1     3 4 2 )"))
//println(logged.mkString("\n"))
    
    println(expr.parse(" ( +  1     3 4 2 )"))

    println(expr.parse(" ( +  1    ( *  3 4 )  2 )"))

    {
      val s = "(+ 1 2 3 )"
      println(s"$s = ${run(s)}")
    }
    
    {
      val s = "(* 1 2 3 )"
      println(s"$s = ${run(s)}")
      
      
    }
    
     {
      val s = "(* (* 1 2) (* 3 4)  )"
      println(s"$s = ${run(s)}")
      
      
    }
     
     {
      val s = "(* (* 1 2) (* 3 4) (- 2 1) ( * 1 1 )   ( /  1  1  ( +  1  (- 3 3 ) ) )  )"
      println(s"$s = ${run(s)}")
      
      
    }
     
     
    {
      val s = "(- 1 2 3 )"
      println(s"$s = ${run(s)}")
    }
    {
      val s = "(/ 1 2 3 )"
      println(s"$s = ${run(s)}")
    }
  }

}