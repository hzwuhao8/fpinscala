package ch9

/**
 * 前缀表达式
 *
 */
import fastparse.all._

object ExamIndentation {

  def eval(tree: (String, Seq[Int])) = {
    println(tree)
    tree match {

      case ("+", nums) => nums.reduceLeft(_ + _)
      case ("-", nums) => nums.reduceLeft(_ - _)
      case ("*", nums) => nums.reduceLeft(_ * _)
      case ("/", nums) => nums.reduceLeft(_ / _)
    }
  }
  class Parser(indent: Int) {
    val number: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))

    val deeper: P[Int] = P(" ".rep(indent + 1).!.map(_.length))
    val blockBody: P[Seq[Int]] = "\n" ~ deeper.flatMap(i =>
      new Parser(indent = i).factor.rep(1, sep = ("\n" + " " * i).~/))
    val block: P[Int] = P(CharIn("+-*/").! ~/ blockBody).map(eval)

    val factor: P[Int] = P(number | block)

    val expr: P[Int] = P(block ~ End)
  }

  val expr = new Parser(indent = 0).expr

  def check(str: String, num: Int) = {
    val Parsed.Success(value, _) = expr.parse(str)
    assert(value == num)
  }

  def main(args: Array[String]) {
    check(
      """+
    |  1
    |  1
  """.stripMargin.trim,
      2)
  }
}
