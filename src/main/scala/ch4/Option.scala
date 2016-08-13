package ch4

import scala.collection.mutable.Buffer

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None    => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case x    => x
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _               => None
  }

  def get(): A = this match {
    case Some(x) => x
    case _       => throw new RuntimeException("xxx")
  }
}
case object None extends Option[Nothing]
case class Some[A](a: A) extends Option[A]

object Option {

  def sequense[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def tt(list: List[A], res: Option[Buffer[B]])(f: A => Option[B]): Option[Buffer[B]] = {
      list match {
        case Nil => res
        case h :: t =>
          println(h)
          val v = f(h)
          v match {
            case None    => None
            case Some(x) => tt(t, Some(res.get += x))(f)
          }

      }
    }
    val r = tt(list, Some(Buffer[B]()))(f)
    r.map(_.toList)
  }

}

object OptionT {
  def main(args: Array[String]): Unit = {
    val t = List(Some(1), Some(2), Some(3), None, Some(4))
    val r = Option.sequense(t)
    println(s"r=$r")

    val r2 = Option.traverse(List("1", "2", "3", "4")) { (x: String) =>
      try {
        Some(x.toInt)
      } catch {
        case ex: Exception => None
      }
    }
    println(s"r2=${r2}")
  }

}