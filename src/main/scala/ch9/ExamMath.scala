package ch9

import fastparse.all._

object ExamMath {

  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) => op match {
        case "+" => left + right 
        case "-" => left - right
        case "*" => left * right 
        case "/" => left / right
        case "%" => left % right
      }
    }
  }

  def main(args: Array[String]) {
    val space: P[Unit] = P(" " | "\t")
    val number1: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))

    val number2: P[Int] = P(space.rep ~ CharIn('0' to '9').rep(1).! ~ space.rep).map(s => s.trim().toInt)

    val number: P[Int] = number2
    lazy val parens: P[Int] = P("(" ~/ addSub ~ ")")
    lazy val factor: P[Int] = P(number | parens)

    lazy val divMul: P[Int] = P(factor ~ (CharIn("*/%").! ~/ factor).rep).map(eval)
    lazy val addSub: P[Int] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(eval)
    
    lazy val expr: P[Int] = P(addSub ~ End)

    def check(str: String, num: Int) = {
      val Parsed.Success(value, _) = expr.parse(str)
      assert(value == num)
    }

    {

      println(number2.parse("1 "))
      println(number2.parse(" 12 "))
      println(number2.parse(" 123"))
    }

    check("1+1", 2)
    check("1+1*2", 3)
    check("(1+1*2)+(3*4*5)", 63)
    check("15/3", 5)
    check("63/3", 21)
    check("(1+1*2)+(3*4*5)/20", 6)
    check("((1+1*2)+(3*4*5))/3", 21)

    check("((1 +1* 2)+(3*4 *5))/3", 21)
    check("((1 +1* 2)+(3 * 4 *5   \t))/3", 21)

     check("1%1", 0)
     check("1+1%1", 1)
     check("(1+1)%1", 0)
     
     check("abc",0)
  }
}