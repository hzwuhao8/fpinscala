package ch9
import ch8.MyGen
import ch8.MyGenX
import ch8.Prop
import scala.util.matching.Regex

class ParseError {

}

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def regex(r: Regex): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n > 0) {
      map2(p, listOfN(n - 1, p))(_ :: _)
    } else {
      succeed(List())
    }
  }

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _) or succeed(List())
  }
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = {
    flatMap(a) { x => succeed(f(x)) }
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many1(p))(_ :: _) or (p.map { x => List(x) })
  }

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    def f(a: A) = { p2.map(b => (a, b)) }
    flatMap(p)(f)
  }
  def **[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = product(p, p2)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    val p3 = product(p, p2)
    p3.map { x => f(x._1, x._2) }
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B) = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    val numA: Parser[Int] = char('a').many.map { _.size }
    def slice = self.slice(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: MyGen[String]): Prop =
      MyGenX.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: MyGen[String]): Prop = equal(p, p.map(a => a))(in)
  }

}